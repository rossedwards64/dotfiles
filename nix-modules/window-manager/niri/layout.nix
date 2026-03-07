{
  flake.modules.homeManager.base.programs.niri.settings.layout = {
    gaps = 16;
    center-focused-column = "on-overflow";
    default-column-width.proportion = 0.5;
    preset-column-widths = [
      { proportion = 1. / 3.; }
      { proportion = 1. / 2.; }
      { proportion = 2. / 3.; }
    ];

    preset-window-heights = [
      { proportion = 1. / 3.; }
      { proportion = 1. / 2.; }
      { proportion = 2. / 3.; }
    ];

    struts = {
      left = 4;
      right = 4;
      top = 4;
      bottom = 4;
    };
  };
}
