{application,rest_store,
             [{description,"REST Interface for backend services"},
              {vsn,"0.0.1"},
              {registered,[rest_store_sup]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{rest_store_app,[]}},
              {env,[]},
              {modules,[index_handler,rest_store,rest_store_app,
                        rest_store_sup]}]}.
