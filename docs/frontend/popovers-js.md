# popovers.js

**Path:** `js/popovers.js` | **Language:** JavaScript | **Lines:** 733

> Mobile/touch pop-frame provider for inline link previews, replacing the old `popins.js` module.

---

## Overview

`popovers.js` implements the mobile/touch pop-frame provider used by `extracts.js`. Desktop clients use `Popups`; mobile clients, or clients with `extracts-force-popovers` set in local storage, use `Popovers`.

The provider exposes the same broad pop-frame interface that `extracts.js` expects: register targets, create a frame for a target, set content, track loading states, scroll content into view, and remove frames during cleanup.

Unlike the removed `popins.js` naming, the current source uses `Popovers`, `.popover`, `spawns-popover`, `preparePopover`, and `spawnedPopovers`.

## Key Behavior

- `Node.prototype.getPopover()` and `setPopover()` store the associated popover under `_gw_popover`, avoiding collision with the native HTML `popover` attribute.
- `Popovers.setup()` runs cleanup, attaches Escape and `popstate` handlers, and fires `Popovers.setupDidComplete`.
- `Popovers.addTarget()` binds click activation, stores `target.preparePopover`, and marks the target with `spawns-popover`.
- `Popovers.injectPopoverForTarget()` creates the frame, calls the target's prepare function, attaches it near the target, updates the spawned stack, and fires `Popovers.popoverDidInject`.
- `Popovers.updateLocationForSpawnedPopovers()` records the current stack in URL/hash history state.
- `Popovers.cleanPopoversFromContainer()` removes stale popovers from content that is about to be replaced.

## Public API

### `setup()` / `cleanup()`

Initialize or tear down the popover provider. Cleanup removes all spawned popovers and unregisters the global handlers.

### `addTarget(target, prepareFunction)`

Registers a target element. The prepare function receives the new popover and is expected to populate its body/title/footer data or return `null` to cancel.

### `removeTarget(target)`

Unregisters a target and removes its currently spawned popover, if any.

### `containingPopFrame(element)`

Finds the containing `.popover`, including the Shadow DOM body path through `.shadow-body`.

### `allSpawnedPopovers()`

Returns the current `spawnedPopovers` stack.

### `scrollElementIntoViewInPopFrame(element, alwaysRevealTopEdge)`

Scrolls the popover body so a target element becomes visible.

### `removePopover(popover, noUpdateLocation)`

Fires `Popovers.popoverWillDespawn`, detaches the popover from its target, removes it from the stack, and optionally updates history state.

## Related Files

- [extracts.js](/frontend/extracts-js) chooses between `Popups` and `Popovers`.
- [extracts-content.js](/frontend/extracts-content-js) provides content handlers consumed by pop-frame providers.
- [popups.js](/frontend/popups-js) is the desktop hover/window provider.

