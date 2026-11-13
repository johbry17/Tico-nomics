# For copyright in footer, returns the current year as a Pandoc string.
return {
  ["current-year"] = function()
    return pandoc.Str(os.date("%Y"))
  end
}