{application,rest_user,
             [{description,"REST Interface for user"},
              {vsn,"0.0.1"},
              {registered,[rest_user_sup,user_handler]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{rest_user_app,[]}},
              {env,[]},
              {modules,[rest_user_app,rest_user_sup,user_handler,
                        user_server]}]}.
