cask 'vimr' do
  version '0.9.0-SNAPSHOT-20160924.1647-77'
  sha256 'bb3f3f2c0e05a2cafa3f750828d4865e18e84ac186a322f64935992276de289d'

  # github.com/qvacua/vimr was verified as official when first introduced to the cask
  url "https://github.com/qvacua/vimr/releases/download/snapshot%2F20160924.1647-77/VimR-0.9.0-SNAPSHOT-20160924.1647-77.tar.bz2"

  name 'VimR'
  homepage 'http://vimr.org/'
  license :gpl

  app 'VimR.app'
  binary "#{appdir}/VimR.app/Contents/Resources/vimr"
end
