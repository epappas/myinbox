{application,rest_store,
             [{description,"Message Index Store"},
              {vsn,"0.0.1"},
              {modules,[index_handler,myredis,proxy_server,qredis,rest_store,
                        rest_store_app,rest_store_sup,user_server]},
              {registered,[rest_store_sup]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{rest_store_app,[]}},
              {env,[]}]}.
