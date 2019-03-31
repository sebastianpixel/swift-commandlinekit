//
//  LineReader.swift
//  CommandLineKit
//
//  Created by Matthias Zenger on 07/04/2018.
//  Copyright © 2018-2019 Google LLC
//  Copyright © 2017 Andy Best <andybest.net at gmail dot com>
//  Copyright © 2010-2014 Salvatore Sanfilippo <antirez at gmail dot com>
//  Copyright © 2010-2013 Pieter Noordhuis <pcnoordhuis at gmail dot com>
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are met:
//
//  * Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
//  * Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
//  * Neither the name of the copyright holder nor the names of its contributors
//    may be used to endorse or promote products derived from this software without
//    specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
//  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
//  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
//  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

import Foundation

public class LineReader {

  public enum Input {
    case move(Direction)
    case character(Character)
    case controlCharacter(ControlCharacters)

    public enum Direction {
      case up, right, down, left, home, end, wordStart, wordEnd
    }
  }

  /// Does this terminal support this line reader?
  public let termSupported: Bool

  /// Terminal type
  public let currentTerm: String

  /// Does the terminal support colors?
  public let fullColorSupport: Bool

  /// If false (the default) any edits by the user to a line in the history will be discarded
  /// if the user moves forward or back in the history without pressing Enter. If true, all
  /// history edits will be preserved.
  public var preserveHistoryEdits = false

  /// The history of previous line reads
  private var history: LineReaderHistory

  /// Temporary line read buffer to handle browsing of histories
  private var tempBuf: String?

  /// A callback for handling line completions
  private var completionCallback: ((String) -> [String])?

  /// A callback for handling hints
  private var hintsCallback: ((String) -> (String, TextProperties)?)?

  /// A callback for handling inputs
  private var inputCallback: ((String, Input) -> Bool)?

  /// A callback for refreshing the buffer
  private var refreshCallback: ((String) -> Void)?

  /// A POSIX file handle for the input
  private let inputFile: Int32

  /// A POSIX file handle for the output
  private let outputFile: Int32

  /// Initializer
  public init?(inputFile: Int32 = STDIN_FILENO,
               outputFile: Int32 = STDOUT_FILENO,
               completionCallback: ((String) -> [String])? = nil,
               hintsCallback: ((String) -> (String, TextProperties)?)? = nil,
               inputCallback: ((String, Input) -> Bool)? = nil,
               refreshCallback: ((String) -> Void)? = nil) {
    self.inputFile = inputFile
    self.outputFile = outputFile
    self.currentTerm = Terminal.current
    if isatty(inputFile) != 1 {
      return nil
    } else {
      self.termSupported = LineReader.supportedBy(terminal: self.currentTerm)
    }
    self.fullColorSupport = Terminal.fullColorSupport(terminal: self.currentTerm)
    self.history = LineReaderHistory()
    self.completionCallback = completionCallback
    self.hintsCallback = hintsCallback
    self.inputCallback = inputCallback
    self.refreshCallback = refreshCallback
  }

  public static var supportedByTerminal: Bool {
    return LineReader.supportedBy(terminal: Terminal.current)
  }

  public static func supportedBy(terminal: String) -> Bool {
    switch terminal {
      case "", "xcode", "dumb", "cons25", "emacs":
        return false
      default:
        return true
    }
  }

  /// Adds a string to the history buffer.
  public func addHistory(_ item: String) {
    self.history.add(item)
  }

  /// Adds a callback for tab completion. The callback is taking the current text and returning
  /// an array of Strings containing possible completions.
  public func setCompletionCallback(_ callback: @escaping (String) -> [String]) {
    self.completionCallback = callback
  }

  /// Adds a callback for hints as you type. The callback is taking the current text and
  /// optionally returning the hint and a tuple of RGB colours for the hint text.
  public func setHintsCallback(_ callback: @escaping (String) -> (String, TextProperties)?) {
    self.hintsCallback = callback
  }

  /// Adds a callback for any input. The callback is taking the buffer at the time of the
  /// input and a case of the Input enum and returning a Boolean indicating whether the
  /// input should have an effect on the buffer.
  public func setInputCallback(_ callback: @escaping (String, Input) -> Bool) {
    self.inputCallback = callback
  }

  /// Timer that helps differentiating single Esc's from Esc's that start an escape sequence.
  private lazy var escapeSequenceTimer = DispatchSource.makeTimerSource(queue: escapeSequenceTimerQueue)
  private let escapeSequenceTimerQueue = DispatchQueue(label: "")
  private var escapeSequenceTimerIsSuspended = true

  /// An internal proxy for inputCallback to facilitate falling back to the default method
  /// and to handle escape sequences.
  private func inputCallbackInternal(_ editState: EditState,
                                     _ input: Input?,
                                     _ executeIfCallbackReturnsTrue: @escaping (EditState) throws -> Void) throws {
    guard let input = input else {
      try executeIfCallbackReturnsTrue(editState)
      return
    }

    // If an escape character is detected, it's not directly forwarded to the input callback
    // but triggers a timer. If the escape character is the first character of an escape sequence
    // the following input cancels the timer. If there is no following input the input callback
    // will be called in the timer's eventHandler.
    switch input {

    case .controlCharacter(.Esc):
      escapeSequenceTimerQueue.sync {
        escapeSequenceTimer.setEventHandler {
          _ = self.inputCallback?(editState.buffer, .controlCharacter(.Esc))
        }
        escapeSequenceTimer.schedule(deadline: .now() + .milliseconds(1), repeating: .never)
        if escapeSequenceTimerIsSuspended {
          escapeSequenceTimerIsSuspended = false
          escapeSequenceTimer.resume()
        }
      }
      try executeIfCallbackReturnsTrue(editState)

    default:
      escapeSequenceTimerQueue.sync {
        if !escapeSequenceTimerIsSuspended {
          escapeSequenceTimerIsSuspended = true
          escapeSequenceTimer.suspend()
        }
      }
      if inputCallback?(editState.buffer, input) ?? true {
        try executeIfCallbackReturnsTrue(editState)
      }
    }
  }

  /// Adds a callback for refreshing the current buffer. The callback is taking the current buffer.
  public func setRefreshCallback(_ callback: @escaping (String) -> Void) {
    self.refreshCallback = callback
  }

  /// Loads history from a file and appends it to the current history buffer. This method can
  /// throw an error if the file cannot be found or loaded.
  public func loadHistory(fromFile path: String) throws {
    try self.history.load(fromFile: path)
  }

  /// Saves history to a file. This method can throw an error if the file cannot be written to.
  public func saveHistory(toFile path: String) throws {
    try self.history.save(toFile: path)
  }

  /// Sets the maximum amount of items to keep in history. If this limit is reached, the oldest
  /// item is discarded when a new item is added. Setting the maximum length of history to 0
  /// (the default) will keep unlimited items in history.
  public func setHistoryMaxLength(_ historyMaxLength: UInt) {
    self.history.maxLength = historyMaxLength
  }

  /// Clears the screen. This method can throw an error if the terminal cannot be written to.
  public func clearScreen() throws {
    if self.termSupported {
      try self.output(text: AnsiCodes.homeCursor)
      try self.output(text: AnsiCodes.clearScreen)
    }
  }

  /// The main function of LineReader. This method shows a prompt to the user at the beginning
  /// of the line and reads the input from the user, returning it as a string. The method can
  /// throw an error if the terminal cannot be written to.
  public func readLine(prompt: String,
                       maxCount: Int? = nil,
                       strippingNewline: Bool = true,
                       promptProperties: TextProperties = TextProperties.none,
                       readProperties: TextProperties = TextProperties.none,
                       parenProperties: TextProperties = TextProperties.none) throws -> String {
    tempBuf = nil
    if self.termSupported {
      return try self.readLineSupported(prompt: prompt,
                                        maxCount: maxCount,
                                        strippingNewline: strippingNewline,
                                        promptProperties: promptProperties,
                                        readProperties: readProperties,
                                        parenProperties: parenProperties)
    } else {
      return try self.readLineUnsupported(prompt: prompt,
                                          maxCount: maxCount,
                                          strippingNewline: strippingNewline)
    }
  }

  private func readLineUnsupported(prompt: String,
                                   maxCount: Int?,
                                   strippingNewline: Bool) throws -> String {
    print(prompt, terminator: "")
    if let line = Swift.readLine(strippingNewline: strippingNewline) {
      return maxCount != nil ? String(line.prefix(maxCount!)) : line
    } else {
      throw LineReaderError.EOF
    }
  }

  private func readLineSupported(prompt: String,
                                 maxCount: Int?,
                                 strippingNewline: Bool,
                                 promptProperties: TextProperties,
                                 readProperties: TextProperties,
                                 parenProperties: TextProperties) throws -> String {
    var line: String = ""
    try self.withRawMode {
      try self.output(text: promptProperties.apply(to: prompt))
      let editState = EditState(prompt: prompt,
                                maxCount: maxCount,
                                promptProperties: promptProperties,
                                readProperties: readProperties,
                                parenProperties: parenProperties)
      var done = false
      while !done {
        guard var char = self.readByte() else {
          return
        }
        if char == ControlCharacters.Tab.rawValue && self.completionCallback != nil,
           let completionChar = try self.completeLine(editState: editState) {
          char = completionChar
        }

        let input = ControlCharacters(rawValue: char).map(Input.controlCharacter)
        try inputCallbackInternal(editState, input) { _ in
          if let rv = try self.handleCharacter(char, editState: editState) {
            if editState.moveEnd() {
              try self.updateCursorPos(editState: editState)
            }
            // It's unclear to me why it's necessary to set the cursor to column 0
            try self.output(text: "\n" + AnsiCodes.setCursorColumn(0))
            line = rv
            done = true
          }
        }
      }
    }
    return strippingNewline ? line : line + "\n"
  }

  private func completeLine(editState: EditState) throws -> UInt8? {
    guard let completionCallback = self.completionCallback else {
      return nil
    }
    let completions = completionCallback(editState.buffer)
    guard completions.count > 0 else {
      self.ringBell()
      return nil
    }
    // Loop to handle inputs
    var completionIndex = 0
    while true {
      if completionIndex < completions.count {
        try editState.withTemporaryState {
          try self.setBuffer(editState: editState, new: completions[completionIndex])
        }
      } else {
        try refreshLine(editState: editState)
      }
      guard let char = self.readByte() else {
        return nil
      }
      switch char {
        case ControlCharacters.Tab.rawValue:
          // Move to next completion
          completionIndex = (completionIndex + 1) % (completions.count + 1)
          if completionIndex == completions.count {
            self.ringBell()
          }
        case ControlCharacters.Esc.rawValue:
          // Show the original buffer
          if completionIndex < completions.count {
            try refreshLine(editState: editState)
          }
          return char
        default:
          // Update the buffer and return
          if completionIndex < completions.count {
            try self.setBuffer(editState: editState, new: completions[completionIndex])
          }
          return char
      }
    }
  }

  private func handleCharacter(_ ch: UInt8, editState: EditState) throws -> String? {
    switch ch {
      case ControlCharacters.Enter.rawValue:
        try refreshLine(editState: editState, decorate: false)
        return editState.buffer
      case ControlCharacters.CtrlA.rawValue:
        try self.moveHome(editState: editState)
      case ControlCharacters.CtrlE.rawValue:
        try self.moveEnd(editState: editState)
      case ControlCharacters.CtrlB.rawValue:
        try self.moveLeft(editState: editState)
      case ControlCharacters.CtrlC.rawValue:
        // Throw an error so that CTRL+C can be handled by the caller
        throw LineReaderError.CTRLC
      case ControlCharacters.CtrlD.rawValue:
        // If there is a character at the right of the cursor, remove it
        if editState.eraseCharacterRight() {
          try self.refreshLine(editState: editState)
        } else {
          self.ringBell()
        }
      case ControlCharacters.CtrlP.rawValue:
        // Previous history item
        try self.moveHistory(editState: editState, direction: .previous)
      case ControlCharacters.CtrlN.rawValue:
        // Next history item
        try self.moveHistory(editState: editState, direction: .next)
      case ControlCharacters.CtrlL.rawValue:
        // Clear screen
        try self.clearScreen()
        try self.refreshLine(editState: editState)
      case ControlCharacters.CtrlT.rawValue:
        if editState.swapCharacterWithPrevious() {
          try refreshLine(editState: editState)
        } else {
          self.ringBell()
        }
      case ControlCharacters.CtrlU.rawValue:
        // Delete whole line
        try self.setBuffer(editState: editState, new: "")
      case ControlCharacters.CtrlK.rawValue:
        // Delete to the end of the line
        if editState.deleteToEndOfLine() {
          try self.refreshLine(editState: editState)
        } else {
          self.ringBell()
        }
      case ControlCharacters.CtrlW.rawValue:
        // Delete previous word
        if editState.deletePreviousWord() {
          try self.refreshLine(editState: editState)
        } else {
          self.ringBell()
        }
      case ControlCharacters.Backspace.rawValue:
        // Delete character
        if editState.backspace() {
          try self.refreshLine(editState: editState)
        } else {
          self.ringBell()
        }
      case ControlCharacters.Esc.rawValue:
        try self.handleEscapeCode(editState: editState)
      default:
        // Read unicode character and insert it at the cursor position using UTF8 encoding
        var scalar = UInt32(ch)
        if ch >> 7 == 0 {
          // done
        } else if ch >> 5 == 0x6 {
          let ch2 = self.forceReadByte()
          scalar = (UInt32(ch & 0x1F) << 6) | UInt32(ch2 & 0x3F)
        } else if ch >> 4 == 0xE {
          let ch2 = self.forceReadByte()
          let ch3 = self.forceReadByte()
          scalar = (UInt32(ch & 0xF) << 12) | (UInt32(ch2 & 0x3F) << 6) | UInt32(ch3 & 0x3F)
        } else if ch >> 3 == 0x1E {
          let ch2 = self.forceReadByte()
          let ch3 = self.forceReadByte()
          let ch4 = self.forceReadByte()
          scalar = (UInt32(ch & 0x7) << 18) |
                   (UInt32(ch2 & 0x3F) << 12) |
                   (UInt32(ch3 & 0x3F) << 6) |
                   UInt32(ch4 & 0x3F)
        }
        let char = Character(UnicodeScalar(scalar) ?? UnicodeScalar(" "))
        if editState.insertCharacter(char) {
          try inputCallbackInternal(editState, .character(char), { try self.refreshLine(editState: $0) })
        } else {
          self.ringBell()
        }
        if self.bytesAvailable > 0 {
          self.ringBell()
        }
    }
    return nil
  }

  private func handleEscapeCode(editState: EditState) throws {
    let fst = self.readCharacter()
    switch fst {
      case ControlCharacters.Esc.character?:
        try inputCallbackInternal(editState, .controlCharacter(.Esc), handleEscapeCode)
        break
      case "[":
        let snd = self.readCharacter()
        switch snd {
          // Handle multi-byte sequence ^[[0...
          case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
            let trd = self.readCharacter()
            switch trd {
              case "~":
                switch snd {
                  case "1", "7":
                    try inputCallbackInternal(editState, .move(.home), moveHome)
                  case "3":
                    try inputCallbackInternal(editState, .controlCharacter(.Backspace), deleteCharacter)
                  case "4":
                    try inputCallbackInternal(editState, .move(.end), moveEnd)
                  default:
                    break
                }
              case ";":
                let fot = self.readCharacter()
                let fth = self.readCharacter()
                // Shift
                if fot == "2" {
                  switch fth {
                    case "C":
                      try inputCallbackInternal(editState, .move(.right), moveRight)
                    case "D":
                      try inputCallbackInternal(editState, .move(.left), moveLeft)
                    default:
                      break
                  }
                }
                break
              case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
                _ = self.readCharacter()
                // ignore these codes for now
                break
              default:
                break
            }
          // ^[...
          case "A":
            try inputCallbackInternal(editState, .move(.up), { try self.moveHistory(editState: $0, direction: .previous) })
          case "B":
            try inputCallbackInternal(editState, .move(.down), { try self.moveHistory(editState: $0, direction: .next) })
          case "C":
            try inputCallbackInternal(editState, .move(.right), moveRight)
          case "D":
            try inputCallbackInternal(editState, .move(.left), moveLeft)
          case "H":
            try inputCallbackInternal(editState, .move(.home), moveHome)
          case "F":
            try inputCallbackInternal(editState, .move(.end), moveEnd)
          default:
            break
        }
      case "O":
        // ^[O...
        let snd = self.readCharacter()
        switch snd {
          case "H":
            try inputCallbackInternal(editState, .move(.home), moveHome)
          case "F":
            try inputCallbackInternal(editState, .move(.end), moveEnd)
          case "P":
            // F1
            break
          case "Q":
            // F2
            break
          case "R":
            // F3
            break
          case "S":
            // F4
            break
          default:
            break
        }
      case "b":
        // Alt+Left
        try inputCallbackInternal(editState, .move(.wordStart), moveToWordStart)
      case "f":
        // Alt+Right
        try inputCallbackInternal(editState, .move(.wordEnd), moveToWordEnd)
      default:
        if let char = fst?.unicodeScalars.first?.value {
          _ = try handleCharacter(UInt8(char), editState: editState)
        }
        break
    }
  }

  private var cursorColumn: Int? {
    do {
      try self.output(text: AnsiCodes.cursorLocation)
    } catch {
      return nil
    }
    var buf = [UInt8]()
    var i = 0
    while true {
      if let c = self.readByte() {
        buf[i] = c
      } else {
        return nil
      }
      if buf[i] == 82 { // "R"
        break
      }
      i += 1
    }
    // Check the first characters are the escape code
    if buf[0] != 0x1B || buf[1] != 0x5B {
      return nil
    }
    let positionText = String(bytes: buf[2..<buf.count], encoding: .utf8)
    guard let rowCol = positionText?.split(separator: ";") else {
      return nil
    }
    if rowCol.count != 2 {
      return nil
    }
    return Int(String(rowCol[1]))
  }

  private var numColumns: Int {
    var winSize = winsize()
    if ioctl(1, UInt(TIOCGWINSZ), &winSize) == -1 || winSize.ws_col == 0 {
      return 80
    } else {
      return Int(winSize.ws_col)
    }
  }

  /// This constant is unfortunately not defined right now for usage in Swift; it is specific
  /// to macOS. Thus, this code is not portable!
  private static let FIONREAD: UInt = 0x4004667f

  private var bytesAvailable: Int {
    var available: Int = 0
    guard ioctl(self.inputFile, LineReader.FIONREAD, &available) >= 0 else {
      return 0
    }
    return available
  }

  private func updateCursorPos(editState: EditState) throws {
    if editState.requiresMatching() {
      try self.refreshLine(editState: editState)
    } else {
      let cursorWidth = editState.cursorWidth
      let numColumns = self.numColumns
      let cursorRows = cursorWidth / numColumns
      let cursorCols = cursorWidth % numColumns
      var commandBuf = AnsiCodes.beginningOfLine
      commandBuf += AnsiCodes.cursorDown(cursorRows)
      commandBuf += AnsiCodes.cursorForward(cursorCols)
      try self.output(text: commandBuf)
    }
  }

  private func refreshLine(editState: EditState, decorate: Bool = true) throws {
    let cursorWidth = editState.cursorWidth
    let numColumns = self.numColumns
    let cursorRows = cursorWidth / numColumns
    let cursorCols = cursorWidth % numColumns
    var commandBuf = AnsiCodes.beginningOfLine +
                     editState.promptProperties.apply(to: editState.prompt)
    if decorate, let idx = editState.matchingParen() {
      var fst = editState.buffer.index(before: editState.location)
      var snd = idx
      if fst > snd {
        snd = fst
        fst = idx
      }
      let one = String(editState.buffer.prefix(upTo: fst))
      let two = String(editState.buffer[editState.buffer.index(after: fst)..<snd])
      let three = String(editState.buffer.suffix(from: editState.buffer.index(after: snd)))
      let highlightProperties = editState.readProperties.with(editState.parenProperties)
      commandBuf += editState.readProperties.apply(to: one)
      commandBuf += highlightProperties.apply(to: String(editState.buffer[fst]))
      commandBuf += editState.readProperties.apply(to: two)
      commandBuf += highlightProperties.apply(to: String(editState.buffer[snd]))
      commandBuf += editState.readProperties.apply(to: three)
    } else {
      commandBuf += editState.readProperties.apply(to: editState.buffer)
    }
    let hints = decorate ? try self.refreshHints(editState: editState) : ""
    commandBuf += hints.isEmpty ? " " : hints
    commandBuf += AnsiCodes.clearCursorToBottom +
                  AnsiCodes.beginningOfLine +
                  AnsiCodes.cursorDown(cursorRows) +
                  AnsiCodes.cursorForward(cursorCols)
    refreshCallback?(editState.buffer)
    try self.output(text: commandBuf)
  }

  private func readByte() -> UInt8? {
    var input: UInt8 = 0
    if read(self.inputFile, &input, 1) == 0 {
      return nil
    }
    return input
  }

  private func forceReadByte() -> UInt8 {
    var input: UInt8 = 0
    _ = read(self.inputFile, &input, 1)
    return input
  }

  private func readCharacter() -> Character? {
    var input: UInt8 = 0
    _ = read(self.inputFile, &input, 1)
    return Character(UnicodeScalar(input))
  }

  private func ringBell() {
    do {
      try self.output(character: ControlCharacters.Bell.character)
    } catch {
      // ignore failure
    }
  }

  private func output(character: ControlCharacters) throws {
    try self.output(character: character.character)
  }

  private func output(character: Character) throws {
    try self.output(text: String(character))
  }

  private func output(text: String) throws {
    if write(outputFile, text, text.utf8.count) == -1 {
      throw LineReaderError.generalError("Unable to write to output")
    }
  }

  private func setBuffer(editState: EditState, new buffer: String) throws {
    if editState.setBuffer(buffer) {
      _ = editState.moveEnd()
      try self.refreshLine(editState: editState)
    } else {
      self.ringBell()
    }
  }

  private func moveLeft(editState: EditState) throws {
    if editState.moveLeft() {
      try self.updateCursorPos(editState: editState)
    }
  }

  private func moveRight(editState: EditState) throws {
    if editState.moveRight() {
      try self.updateCursorPos(editState: editState)
    }
  }

  private func moveHome(editState: EditState) throws {
    if editState.moveHome() {
      try self.updateCursorPos(editState: editState)
    } else {
      self.ringBell()
    }
  }

  private func moveEnd(editState: EditState) throws {
    if editState.moveEnd() {
      try self.updateCursorPos(editState: editState)
    } else {
      self.ringBell()
    }
  }

  private func moveToWordStart(editState: EditState) throws {
    if editState.moveToWordStart() {
      try self.updateCursorPos(editState: editState)
    } else {
      self.ringBell()
    }
  }

  private func moveToWordEnd(editState: EditState) throws {
    if editState.moveToWordEnd() {
      try self.updateCursorPos(editState: editState)
    } else {
      self.ringBell()
    }
  }

  private func deleteCharacter(editState: EditState) throws {
    if editState.deleteCharacter() {
      try self.refreshLine(editState: editState)
    }
  }

  private func moveHistory(editState: EditState,
                           direction: LineReaderHistory.HistoryDirection) throws {
    // If we're at the end of history (editing the current line), push it into a temporary
    // buffer so it can be retrieved later
    if self.history.currentIndex == self.history.historyItems.count {
      tempBuf = editState.buffer
    } else if self.preserveHistoryEdits {
      self.history.replaceCurrent(editState.buffer)
    }
    if let historyItem = self.history.navigateHistory(direction: direction) {
      try self.setBuffer(editState: editState, new: historyItem)
    } else if case .next = direction {
      try self.setBuffer(editState: editState, new: tempBuf ?? "")
    } else {
      self.ringBell()
    }
  }

  private func refreshHints(editState: EditState) throws -> String {
    guard let hintsCallback = self.hintsCallback,
          let (hint, properties) = hintsCallback(editState.buffer) else {
      return ""
    }
    let currentLineLength = editState.prompt.count + editState.buffer.count
    if hint.count + currentLineLength > self.numColumns {
      return ""
    } else {
      return properties.apply(to: hint) + AnsiCodes.origTermColor
    }
  }

  private func withRawMode(body: () throws -> ()) throws {
    var originalTermios: termios = termios()
    defer {
      _ = tcsetattr(self.inputFile, TCSADRAIN, &originalTermios)
    }
    if tcgetattr(self.inputFile, &originalTermios) == -1 {
      throw LineReaderError.generalError("could not get term attributes")
    }
    var raw = originalTermios
    raw.c_iflag &= ~tcflag_t(BRKINT | ICRNL | INPCK | ISTRIP | IXON)
    raw.c_oflag &= ~tcflag_t(OPOST)
    raw.c_cflag |= tcflag_t(CS8)
    raw.c_lflag &= ~tcflag_t(ECHO | ICANON | IEXTEN | ISIG)
    // VMIN = 16
    raw.c_cc.16 = 1
    guard tcsetattr(self.inputFile, TCSADRAIN, &raw) >= 0 else {
      throw LineReaderError.generalError("Could not set raw mode")
    }
    try body()
  }
}
