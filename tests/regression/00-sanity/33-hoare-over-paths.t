  $ goblint --set ana.path_sens[+] mutex --set result pretty --set outfile pretty.txt 33-hoare-over-paths.c
  [Success][Assert] Assertion "1" will succeed (33-hoare-over-paths.c:11:5-11:24)
  [Success][Assert] Assertion "1" will succeed (33-hoare-over-paths.c:16:5-16:24)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total: 7

  $ cat pretty.txt
  Mapping {
    33-hoare-over-paths.c:7:1-34:1(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   })}
    33-hoare-over-paths.c:9:7-9:8(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Local {
                                                                                 r ->
                                                                                   (Unknown int([-31,31]))
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   })}
    33-hoare-over-paths.c:10:5-10:10(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Local {
                                                                                 r ->
                                                                                   (Not {0}([-31,31]))
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   })}
    33-hoare-over-paths.c:11:5-11:24(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Local {
                                                                                 r ->
                                                                                   (0)
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   })}
    33-hoare-over-paths.c:15:5-15:27(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Local {
                                                                                 r ->
                                                                                   (0)
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   })}
    33-hoare-over-paths.c:16:5-16:24(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{m}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Local {
                                                                                 r ->
                                                                                   (0)
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   })}
    33-hoare-over-paths.c:33:3-33:11(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{m}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Local {
                                                                                 r ->
                                                                                   (0)
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   }), ([Unit:(),
                                                                         Unit:(),
                                                                         Unit:(),
                                                                         Reversed (top or Set (Normal Lvals * booleans)):{},
                                                                         Unit:(),
                                                                         Unit:(),
                                                                         top or Set (variables):{},
                                                                         booleans:False,
                                                                         MT mode:Singlethreaded,
                                                                         Thread * lifted created and Unit:([main], bot),
                                                                         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                                                                                               Global {
                                                                                                                                                 m ->
                                                                                                                                                   mutex
                                                                                                                                               }
                                                                                                                                               Local {
                                                                                                                                                 r ->
                                                                                                                                                   (0)
                                                                                                                                               }
                                                                                                                                             }, mapping {
                                                                                                                                                    }, {}, {}),
                                                                         top or std * lifted node:(mapping {
                                                                                                     }, Unknown node),
                                                                         Unit:()], mapping {
                                                                                     })}
    33-hoare-over-paths.c:7:1-34:1(main) ->
      {([Unit:(), Unit:(), Unit:(),
         Reversed (top or Set (Normal Lvals * booleans)):{m}, Unit:(), Unit:(),
         top or Set (variables):{}, booleans:False, MT mode:Singlethreaded,
         Thread * lifted created and Unit:([main], bot),
         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                               Global {
                                                                                 m ->
                                                                                   mutex
                                                                               }
                                                                               Temp {
                                                                                 RETURN ->
                                                                                   (0)
                                                                               }
                                                                             }, mapping {
                                                                                    }, {}, {}),
         top or std * lifted node:(mapping {
                                     }, Unknown node), Unit:()], mapping {
                                                                   }), ([Unit:(),
                                                                         Unit:(),
                                                                         Unit:(),
                                                                         Reversed (top or Set (Normal Lvals * booleans)):{},
                                                                         Unit:(),
                                                                         Unit:(),
                                                                         top or Set (variables):{},
                                                                         booleans:False,
                                                                         MT mode:Singlethreaded,
                                                                         Thread * lifted created and Unit:([main], bot),
                                                                         value domain * array partitioning deps * Vars with Weak Update * P:(mapping {
                                                                                                                                               Global {
                                                                                                                                                 m ->
                                                                                                                                                   mutex
                                                                                                                                               }
                                                                                                                                               Temp {
                                                                                                                                                 RETURN ->
                                                                                                                                                   (0)
                                                                                                                                               }
                                                                                                                                             }, mapping {
                                                                                                                                                    }, {}, {}),
                                                                         top or std * lifted node:(mapping {
                                                                                                     }, Unknown node),
                                                                         Unit:()], mapping {
                                                                                     })}
    OTHERS -> Not available
  }
