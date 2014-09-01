{application,rest_susr,
             [{description,"REST Interface for susr"},
              {vsn,"0.0.1"},
              {registered,[rest_susr_sup,susr_handler]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{rest_susr_app,[]}},
              {env,[]},
              {modules,[rest_susr_app,rest_susr_sup,susr_handler,
                        susr_server]}]}.
