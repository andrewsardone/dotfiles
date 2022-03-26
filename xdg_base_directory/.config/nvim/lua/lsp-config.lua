-- Configuration and setup of Language Server Protocol (LSP)

local servers = { 'tsserver' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {}
end
