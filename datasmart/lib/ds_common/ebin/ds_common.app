{application,ds_common,
             [{description,"Common utils"},
              {vsn,"0.0.1"},
              {registered,[qredis,hash_md5,ds_util]},
              {applications,[kernel,stdlib]},
              {mod,{rest_common_app,[]}},
              {env,[]},
              {modules,[ds_util,hash_md5,qredis,rest_common_app]}]}.
