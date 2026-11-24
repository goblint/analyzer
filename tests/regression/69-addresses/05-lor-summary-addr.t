Query the disjunction directly: CIL lowers || in an ordinary __goblint_check
into separate branches. All three allocations share a summary address, but
neither equality is true. Unknown or false is sound; true is not.

  $ ruby -rjson <<'RUBY'
  > file = '05-lor-summary-addr.c'
  > line = File.readlines(file).index { |s| s.include?('return 0;') } + 1
  > IO.popen(['goblint', '--enable', 'server.enabled', '--enable', 'exp.arg.enabled',
  >           '--disable', 'warn.deadcode', '--disable', 'sem.malloc.fail',
  >           '--set', 'ana.malloc.unique_address_count', '0', file], 'r+') do |io|
  >   request = lambda do |method, params|
  >     io.puts JSON.generate(jsonrpc: '2.0', id: 1, method: method, params: params)
  >     JSON.parse(io.readline).fetch('result')
  >   end
  >   raise 'analysis failed' unless request.call('analyze', {}).fetch('status') == ['Success']
  >   nodes = request.call('arg/lookup', location: {file: file, line: line, column: 3})
  >   raise 'return is unreachable' if nodes.empty?
  >   nodes.each do |n|
  >     result = request.call('arg/eval-int', node: n.fetch('node'), exp: 'p == q || p == r')
  >     puts 'Incorrectly proved p == q || p == r' if result.fetch('bool') == true
  >   end
  > end
  > RUBY
