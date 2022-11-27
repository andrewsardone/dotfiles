-- prose.lua
-- A nicer reading and navigating experience (e.g., j & k move up and down wrapped lines) for prose.

local prose_is_toggled = false

local function toggle()
  local keymap = vim.keymap.set
  local opts = { silent = true }
  if prose_is_toggled then
    prose_is_toggled = false
    vim.opt.wrap = false
    keymap("n", "j", "j", opts)
    keymap("n", "k", "k", opts)
  else
    prose_is_toggled = true
    vim.opt.wrap = true
    vim.opt.linebreak = true
    vim.opt.list = false
    keymap("n", "j", "gj", opts)
    keymap("n", "k", "gk", opts)
  end
end

vim.api.nvim_create_user_command("ProseToggle", function()
    toggle()
  end,
  { nargs = 0, desc = "Toggle mode for nicer prose navigation" }
)
