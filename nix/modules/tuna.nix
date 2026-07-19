{ ... }:
{
  system.defaults.CustomUserPreferences = {
    "com.apple.symbolichotkeys" = {
      AppleSymbolicHotKeys = {
        # Spotlight search: rebind from Cmd+Space (Tuna owns that) to
        # Option+Space.  ascii=32 keycode=49 mods=option(524288)
        "64" = {
          enabled = true;
          value = {
            parameters = [ 32 49 524288 ];
            type = "standard";
          };
        };
      };
    };
  };
}
