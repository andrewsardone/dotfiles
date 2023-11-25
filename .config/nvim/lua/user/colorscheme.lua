vim.opt.background = "dark"
local colorscheme = "carbonfox"

local uname = vim.fn.system("uname")
if string.find(uname, "Darwin") then
  local interface_style = vim.fn.system("defaults read -g AppleInterfaceStyle 2>&1")
  if string.find(interface_style, "Dark") then
    vim.opt.background = "dark"
    colorscheme = "carbonfox"
  else
    vim.opt.background = "light"
    colorscheme = "dayfox"
  end
end

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  return
end

-- Set up EdenEast/nightfox.nvim
local nightfox_status_ok, nightfox = pcall(require, "nightfox")
if nightfox_status_ok then
  nightfox.setup({
    palettes = {
      carbonfox = {
        sel0 = "#3F3F3F",
        bg4 = "#FFFFFF",
      },
      dayfox = {
        bg1 = "#FFFFFF",
      }
    },
    options = {
      dim_inactive = false,
      styles = {
        comments = "italic",

      }
    }
  })
end
