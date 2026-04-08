function Pandoc(doc)
  -- 1. Keep explicit metadata title if present
  if doc.meta.title then
    return doc
  end

  -- 2. Use first level-1 heading
  for _, b in ipairs(doc.blocks) do
    if b.t == "Header" and b.level == 1 then
      doc.meta.title = pandoc.MetaInlines(b.content)
      return doc
    end
  end

  -- 3. Use input filename if available and not stdin
  local input_files = PANDOC_STATE.input_files
  if input_files and input_files[1] and input_files[1] ~= "-" then
    local fname = input_files[1]
    local base = fname:match("([^/]+)%.%w+$") or fname:match("([^/]+)$") or fname
    base = base:gsub("_", " ")
    doc.meta.title = pandoc.MetaString(base)
    return doc
  end

  -- 4. Fallback
  doc.meta.title = pandoc.MetaString("Document")
  return doc
end
