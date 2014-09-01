{application,rest_message,
             [{description,"REST Interface for message"},
              {vsn,"0.0.1"},
              {registered,[rest_message_sup,message_handler,inbox_handler]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{rest_message_app,[]}},
              {env,[]},
              {modules,[inbox_handler,message_handler,message_server,
                        rest_message_app,rest_message_sup]}]}.
