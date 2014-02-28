// key = $1
$1 = fopen($3, $4); // map[$1] = ($1, ["fopen", [$3, $4]])
fprintf($1, $_);    // map[$1] = ($1, ["fopen", [$3, $4]; "fprintf", [$1, $_]])
//  if(!fopen) warn("writing to unopened file")
fclose($1);         // map[$1] = ($1, ["fopen", [$3, $4]; "fprintf", [$1, $_]; "fclose", [$1]])
//  if(!fopen) warn("closing unopened file")

// end
//  for(!fclose) warn("unclosed file")
