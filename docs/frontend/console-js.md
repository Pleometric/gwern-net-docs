
# console.js

**Path:** `js/console.js` | **Language:** JavaScript | **Lines:** ~275

> In-browser developer console with REPL, command history, and output buffer

---

## Overview

console.js implements a custom in-browser developer console for gwern.net, providing a REPL-style interface for debugging and executing JavaScript commands. The console appears as an overlay panel that can be toggled with keyboard shortcuts.

The module creates a visual console view with a scrollable output area and a command-line input field. It supports both a simple built-in command system (like `clear`) and direct JavaScript execution by wrapping commands in backticks. The implementation includes a full command history system with up/down arrow navigation, similar to terminal shells.

The console is disabled by default and must be explicitly enabled via URL query parameter (`?console=1` or `?console=2`) or localStorage setting. This keeps the debugging interface hidden from regular users while remaining accessible to developers.

---

## Public API

### `GW.console.print(entity, flush = true)`

Outputs content to the console. Handles multiple entity types: strings, Error objects, DOM Elements, and arbitrary objects (serialized as JSON). Applies color styling based on type (gray for null/undefined, red for errors). Also mirrors output to the browser's native console.

**Called by:** `commandLineCommandReceived`, temp buffer flush, external debugging code
**Calls:** `flushBuffer`

---

### `GW.console.show()`

Makes the console visible and focuses the input field.

**Called by:** `keyUp` handler (backtick key), external code
**Calls:** `scrollToBottom`

---

### `GW.console.hide()`

Hides the console and blurs the input field.

**Called by:** `keyUp` handler (Escape key)
**Calls:** none

---

### `GW.console.isVisible() -> boolean`

Returns whether the console is currently visible.

**Called by:** `keyDown`, `keyUp`, `show`
**Calls:** none

---

### `GW.console.clearOutput()`

Removes all content from the console output view.

**Called by:** `execLine` ("clear" command)
**Calls:** `updateHeight`, `scrollToBottom`

---

### `GW.console.setPrompt(string)`

Sets the command line prompt text. Initialized to `location.pathname`.

**Called by:** initialization code
**Calls:** none

---

## Internal Architecture

### Data Structures

```javascript
GW.console = {
    outputBuffer: DocumentFragment,  // Buffered output before flush
    view: Element,                   // Main console DOM container
    commandLog: [],                  // Command history array
    commandLog_pointer: number,      // Current position in history
    commandLog_currentCommandLine: string  // Saved partial input
}
```

### View Structure

```
#console.hidden
├── .console-scroll-view
│   └── .console-content-view      // Output paragraphs go here
└── .console-command-line
    ├── .console-command-line-prompt
    │   └── <span>                 // Prompt text (pathname)
    └── .console-command-line-entry-field
        └── <input>                // Command input
```

### Control Flow

1. **Initialization:** `doWhenBodyExists` callback constructs DOM, sets up event listeners if console is enabled
2. **Show/Hide:** Backtick (`) shows console, Escape hides it
3. **Input:** User types command, presses Enter
4. **Execution:** `commandLineCommandReceived` → `execLine` (built-in) or `jsExecLine` (JavaScript)
5. **Output:** `print()` formats entity, appends to buffer, flushes to DOM

---

## Key Patterns

### Backtick-Wrapped JavaScript Execution

Commands wrapped in backticks are treated as JavaScript and passed to the `$()` function (GW's shorthand evaluator). This allows quick expression evaluation:

```javascript
if (/^`.*`$/.test(inputLine))
    GW.console.jsExecLine(inputLine.slice(1, -1));
```

### Shell-Style Command History

The history system preserves partial input when navigating:

```javascript
if (GW.console.commandLog_pointer == GW.console.commandLog.length)
    GW.console.commandLog_currentCommandLine = GW.console.view.input.value;
```

This saves the current (unsent) command before navigating back through history, restoring it when the user returns to the "present."

### Entity-Type Formatting

`print()` detects input type and formats accordingly:
- **Errors:** Red text, stack trace, `console.error()`
- **Strings:** HTML-escaped, `console.log()`
- **Elements:** `outerHTML` displayed, HTML-escaped
- **Objects:** JSON stringified with newlines and tabs preserved

### Temporary Buffer System

Before the console DOM exists, output can be buffered to `GW.consoleTempBuffer`. On initialization, this is flushed:

```javascript
if (GW.consoleTempBuffer > "") {
    GW.consoleTempBuffer.split("\n").forEach(line => {
        GW.console.print(line, false);
    });
}
```

---

## Configuration

| Setting | Location | Effect |
|---------|----------|--------|
| `?console=1` | URL query | Enables console (keyboard listeners) |
| `?console=2` | URL query | Enables console + auto-shows on load |
| `console-enabled=true` | localStorage | Persists console enablement |

The console view is always constructed, but event listeners are only attached when explicitly enabled.

---

## Integration Points

### GW Namespace

- **`GW.console`** - Main namespace for all console functionality
- **`GW.consoleTempBuffer`** - Pre-initialization output buffer (defined elsewhere)

### Helper Functions Used

- **`newDocument()`** - Creates DocumentFragment for output buffer
- **`newElement()`** - Creates DOM elements with attributes
- **`addUIElement()`** - Adds console to page UI layer
- **`doWhenBodyExists()`** - Deferred initialization
- **`getQueryVariable()`** - URL parameter parsing

### CSS Variables

- **`--GW-console-view-height`** - Set dynamically via `updateHeight()` for layout calculations

---

## See Also

- [initial.js](/frontend/initial-js) - GW namespace and core utilities
- [rewrite.js](/frontend/rewrite-js) - DOM transformation system
- [utility.js](/frontend/utility-js) - DOM helpers like `newElement()` and `newDocument()`
- [misc.js](/frontend/misc-js) - Related utility modules and UI components
- [popups.js](/frontend/popups-js) - Popup system that may log to console
- [extracts.js](/frontend/extracts-js) - Content extraction with debug output
