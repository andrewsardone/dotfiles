# Neovim Configuration

Based on [LunarVim/nvim-basic-ide]

[LunarVim/nvim-basic-ide]: https://github.com/LunarVim/nvim-basic-ide/tree/3d3b07f3d8c824d30415783633f11515ee230d41

## Usage

1. Sets <leader> key to <Space>, so it makes heave use of the Spacemacs/SpaceVim-style commands.
2. Check out [keymaps.lua](./lua/user/keymaps.lua) for my core keybinding tweaks, and [handlers.lua](./lua/user/lsp/handlers.lua) for LSP keybindings.

## Get healthy

Open `nvim` and enter the following:

```
:checkhealth
```

## Configuration

### LSP

To add a new LSP

First Enter:

```
:Mason
```

and press `i` on the Language Server you wish to install

Next you will need to add the server to this list: [servers](./lua/user/mason.lua)

Note: Builtin LSP doesn't contain all lsps from [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#terraform_lsp).

If you want to install any from there, for example terraform_lsp (which adds more functionality than terraformls, like complete resource listing),

1. You can add the lsp name in [mason lsp block](./lua/user/mason.lua#L1-L10)

```lua
-- lua/usr/lsp/mason.lua
local servers = {
	"sumneko_lua",
	"cssls",
	"html",
	"tsserver",
	"pyright",
	"bashls",
	"jsonls",
	"yamlls",
  "terraform_lsp" -- New LSP
}
```

2. Manually install the binary of the lsp and put it in your path by downloading the binary or through your package manager. For terraform_lsp [example](https://github.com/juliosueiras/terraform-lsp/releases)

### Formatters and linters

Make sure the formatter or linter is installed and add it to this setup function: [null-ls](./lua/user/lsp/null-ls.lua#L12)

**NOTE** Some are already setup as examples, remove them if you want

### Plugins

You can install new plugins here: [plugins](./lua/user/plugins.lua#L45)
