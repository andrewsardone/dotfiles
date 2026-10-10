const chrome = (profile) => ({ name: "Google Chrome", profile });

export default {
  defaultBrowser: chrome("Default"), // "Google" profile
  handlers: [
    {
      match: [
        "instagram.com/*",
        "*.instagram.com/*",
        "facebook.com/*",
        "*.facebook.com/*",
      ],
      browser: chrome("Profile 1"), // "Social" profile
    },
  ],
};
