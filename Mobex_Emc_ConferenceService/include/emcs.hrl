-define(EMCS_CONFIG_PATH,"emcs.cfg").

-record(database_params, {username          = <<"root">>,
                      password          = <<"password">>,
                      host              = "192.168.1.143",
                      port              = 3306,
                      client_properties = []}).