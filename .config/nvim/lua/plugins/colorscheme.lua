return {
  -- add nightfox
  { "EdenEast/nightfox.nvim" },

  -- Configure LazyVim to load nightfox
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "carbonfox",
    },
  },

  -- tweak nightfox settings
  {
    "EdenEast/nightfox.nvim",
    opts = {
      palettes = {
        carbonfox = {
          sel0 = "#3F3F3F",
          bg4 = "#FFFFFF",
        },
      },
      options = {
        dim_inactive = false,
        styles = {
          comments = "italic",
        },
      },
    },
  },
}
