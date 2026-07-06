(* Behavior-preserving facade. The implementation is split across focused
   sub-modules that chain via `include`:
     Background_task_0_format  (types + format/path helpers)
       -> Background_task_db       (sqlite persistence: schema, queue, CRUD,
                                    status updates, enqueue, counts)
       -> Background_task_log      (log read/render/follow + result
                                    classification + wait + finalize)
       -> Background_task_control  (lifecycle ops: resume, completion pass,
                                     cancel, retry, recover)
       -> Background_task_context  (context/origin routing and delegate prompt)
       -> Background_task_spawn    (process spawning, worktrees, scheduling)
   Including the tail re-exports the entire Background_task.* public surface. *)
include Background_task_spawn
