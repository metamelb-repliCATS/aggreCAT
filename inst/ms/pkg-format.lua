-- -- jss-pkg.lua
-- function Span(el)
--   -- Check if the span has the class 'pkg'
--   if el.classes:includes("pkg") then
--     if quarto.doc.is_format("pdf") then
--       -- In PDF, wrap content in the LaTeX \pkg{} command
--       return {
--         pandoc.RawInline('latex', '\\pkg{'),
--         table.unpack(el.content),
--         pandoc.RawInline('latex', '}')
--       }
--     else
--       -- For other formats (HTML, etc.), fallback to bold or code style
--       return pandoc.Strong(el.content)
--     end
--   end
-- end

-- these classes, when placed on a span will be replaced
 -- with an identical LaTeX command for PDF output
 local texMappings = {
  "proglang",
  "pkg",
  "fct",
  "class"
}

return {
  {
    Span = function(el)
      -- read the span contents and emit correct output
      local contentStr = pandoc.utils.stringify(el.content)

      for i, mapping in ipairs(texMappings) do
        if #el.attr.classes == 1 and el.attr.classes:includes(mapping) then
          if quarto.doc.isFormat("pdf") then
            return pandoc.RawInline("tex", "\\" .. mapping .. "{" .. contentStr .. "}" )
          else
            return pandoc.Code(contentStr);
          end
        end
      end
    end
  }
}
