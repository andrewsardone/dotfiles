vim.opt.background = "dark"
local colorscheme = "carbonfox"

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
