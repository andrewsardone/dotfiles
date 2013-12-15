var fullScreenOp = S.op("move", {
  "x": "screenOriginX", "y": "screenOriginY",
  "width": "screenSizeX", "height": "screenSizeY"
});

// Configuration
S.configAll({
  "modalEscapeKey": "esc"
});
// Bindings
S.bindAll({
  // Grid
  "g:ctrl,shift": S.op("grid", {
    "padding": 5,
    "grids": {
      "1440x900": { "width": 8, "height": 4 }
    }
  }),

  // Resizing windows by anchoring corners
  // This mimics my MercuryMover[1] binds[2] that I've used for years.
  // [1]: http://www.heliumfoot.com/mercurymover/
  // [2]: http://cl.ly/image/3Z2h05061Q2c

  // Resize Left/Up
  "right:left,ctrl,shift,cmd:toggle": S.op("resize", { "width": "-10%", "height": "+0", "anchor": "top-right" }),
  "down:left,ctrl,shift,cmd:toggle": S.op("resize", { "width": "+0", "height":"+10%", "anchor": "top-right" }),
  "left:left,ctrl,shift,cmd:toggle": S.op("resize", { "width": "+10%", "height": "+0", "anchor": "top-right" }),
  "up:left,ctrl,shift,cmd:toggle": S.op("resize", { "width": "+0", "height": "-10%", "anchor": "top-right" }),
  "g:left,ctrl,shift,cmd": fullScreenOp,

  // Resize Right/Down
  "right:right,ctrl,shift,cmd:toggle": S.op("resize", { "width": "+10%", "height": "+0", "anchor": "top-left" }),
  "down:right,ctrl,shift,cmd:toggle": S.op("resize", { "width": "+0", "height":"+10%", "anchor": "top-left" }),
  "left:right,ctrl,shift,cmd:toggle": S.op("resize", { "width": "-10%", "height": "+0", "anchor": "top-left" }),
  "up:right,ctrl,shift,cmd:toggle": S.op("resize", { "width": "+0", "height": "-10%", "anchor": "top-left" }),
  "g:right,ctrl,shift,cmd": fullScreenOp,

  // Window hints
  "esc:cmd,shift": S.op("hint"),

  // window corners and splits
  "h:up,ctrl,shift,cmd": S.op("corner", {
    "direction" : "top-left",
    "width" : "screenSizeX/2",
    "height" : "screenSizeY"
  }),
  "j:up,ctrl,shift,cmd": S.op("corner", {
    "direction" : "bottom-left",
    "width" : "screenSizeX",
    "height" : "screenSizeY/2"
  }),
  "k:up,ctrl,shift,cmd": S.op("corner", {
    "direction" : "top-left",
    "width" : "screenSizeX",
    "height" : "screenSizeY/2"
  }),
  "l:up,ctrl,shift,cmd": S.op("corner", {
    "direction" : "top-right",
    "width" : "screenSizeX/2",
    "height" : "screenSizeY"
  }),
  "g:up,ctrl,shift,cmd": fullScreenOp,
});
