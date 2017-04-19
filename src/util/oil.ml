type cpu = { (* root of an oil file, container for objects *)
  os: os; (* config, 1 *)
  appmodes: appmode list; (* application config, >1 *)
  tasks: task list;
  counters: counter list;
  interrupts: isr list;
}
and obj = Os of os | Appmode of os | Task of task | Counter of counter | Alarm of alarm | Resource of resource | Event of event | Isr of isr | Message of message | Com of unit | Ipdu of unit | Nm of unit
and os = {
  status: [`STANDARD | `EXTENDED];
  errorhook: bool;
  pretaskhook: bool;
  startuphook: bool;
  shutdownhook: bool;
  userrescheduler: bool; (* whether the resource RES_SCHEDULER is used within the application *)
}
and appmode = { name: string } (* TODO *)
and task = {
  priority: int; (* This value has to be understood as a relative value, i.e. the values of PRIORITY show only the relative ordering of the tasks. OSEK OS defines the lowest priority as zero (0); larger values of the PRIORITY attribute correspond to higher priorities. *)
  schedule: [`NON | `FULL]; (* The FULL value of this attribute corresponds to a preemptable task, the NON value to a non- preemptable task. *)
  activation: int; (* maximum number of queued activation requests for the task. A value equal to "1" means that at any time only a single activation is permitted for this task *)
  autostart: [`FALSE | `TRUE of appmode list]; (* When set to TRUE, a list of application modes is defined in the APPMODE sub-attribute of type APPMODE_TYPE. These define in which application modes the task is auto-started. *)
  resources: resource list;
  events: event list;
  messages: message list;
}
and counter = {
  maxallowedvalue: int; (* max counter value *)
  ticksperbase: int; (* number of ticks required to reach a counter-specific unit. The interpretation is implementation-specific. *)
  mincycle: int; (* minimum allowed number of counter ticks for a cyclic alarm linked to the counter. *)
}
and alarm = {
  counter: counter;
  action: [`ACTIVATETASK of task | `SETEVENT of task * event | `ALARMCALLBACK of string];
  alarm_autostart: [`FALSE | `TRUE of alarm_autostart];
}
(* inline records not allowed for polymorphic variants *)
and alarm_autostart = { alarmtime: int; cycletime: int; appmode: appmode list }
and resource = {
  resourceproperty: [
    | `STANDARD
    | `LINKED (* A resource that is linked to another resource with the property STANDARD or LINKED. The resource to which the linking shall be performed is defined by the sub- attribute LINKEDRESOURCE of type RESOURCE_TYPE. *)
    | `INTERNAL (* An internal resource that cannot be accessed by the application. *)
  ]
}
and event = {
  mask: [`Bits of int64 | `AUTO]; (* Bits is no Enum! Value is AUTO or e.g. hex. *)
}
and isr = { (* interrupt service routines *)
  category: int; (* only values of 1and 2 are allowed *)
  isr_resources: resource list;
  isr_messages: message list;
}
and message = { (* TODO *)
  messageproperty: [`SEND | `RECEIVE];
  notification: message_notification;
  notificationerror: message_notification;
}
and message_notification = [`NONE | `ACTIVATETASK of task | `SETEVENT of task * event | `COMCALLBACK of string | `FLAG of string | `INMCALLBACK]