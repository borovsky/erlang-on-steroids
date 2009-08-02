{application, steroids, 
 [{description, "New web framework"},
  {vsn, "0.1"},
  {modules, [s_conf, s_dispatcher, s_internal_types, s_multipart_inets,
             s_platform_inets, s_routes, s_start, s_utils]},
  {registred, [s_conf]},
  {applications, [kernel, stdlib]},
  {mod, {s_app, []}},
  {start_phases, []}
 ]}.
