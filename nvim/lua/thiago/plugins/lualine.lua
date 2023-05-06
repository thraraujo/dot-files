-- import lualine plugin safely
local status, lualine = pcall(require, "lualine")
if not status then
  return
end

-- I will write the custom theme for nord following the official documentation
-- https://www.nordtheme.com/docs/colors-and-palettes
local lualine_nord = require("lualine.themes.nord")

-- new colors for theme - this is a custom theme for nord
local new_colors = {
  blue = "#81a1c1",
  wine = "#b48ead",
  green = "#8fbcbb",
  gray = "#d8dee9",
  black = "#3b4252",
}

-- change nord theme colors
lualine_nord.normal.a.bg = new_colors.blue
lualine_nord.insert.a.bg = new_colors.green
lualine_nord.visual.a.bg = new_colors.gray
lualine_nord.command = {
  a = {
    gui = "bold",
    bg = new_colors.wine,
    fg = new_colors.black, 
  },
}

-- configure lualine with modified theme
lualine.setup({
  options = {
    theme = lualine_nord,
  },
})
