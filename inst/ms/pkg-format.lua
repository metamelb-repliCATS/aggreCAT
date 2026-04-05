-- jss-pkg.lua
function Span(el)
  -- Check if the span has the class 'pkg'
  if el.classes:includes("pkg") then
    if quarto.doc.is_format("pdf") then
      -- In PDF, wrap content in the LaTeX \pkg{} command
      return {
        pandoc.RawInline('latex', '\\pkg{'),
        table.unpack(el.content),
        pandoc.RawInline('latex', '}')
      }
    else
      -- For other formats (HTML, etc.), fallback to bold or code style
      return pandoc.Strong(el.content)
    end
  end
end
