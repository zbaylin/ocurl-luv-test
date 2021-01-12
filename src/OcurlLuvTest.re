module M = Curl.Multi;

let mt = M.create();

let socketPollHandleTbl: Hashtbl.t(Unix.file_descr, Luv.Poll.t) =
  Hashtbl.create(32);

let rec checkInfo = () => {
  switch (M.remove_finished(mt)) {
  | Some((handle, _)) =>
    Printf.printf("DONE: %s\n", Curl.get_effectiveurl(handle));
    checkInfo();
  | None => ()
  };
};

let perform =
    (
      fd: Unix.file_descr,
      res: Result.t(list(Luv.Poll.Event.t), Luv.Error.t),
    ) => {
  let events = Result.get_ok(res);
  let flag =
    switch (events) {
    | [`WRITABLE, `READABLE] => M.EV_INOUT
    | [`WRITABLE] => M.EV_IN
    | [`READABLE] => M.EV_OUT
    | _ => M.EV_AUTO
    };

  let _: int = M.action(mt, fd, flag);

  checkInfo();
};

let handleSocket = (loop: Luv.Loop.t, fd: Unix.file_descr, action: M.poll) => {
  switch (action) {
  | M.POLL_IN
  | M.POLL_OUT
  | M.POLL_INOUT =>
    let pollHandle =
      switch (Hashtbl.find_opt(socketPollHandleTbl, fd)) {
      | Some(poll) => poll
      | None =>
        let luvSocket: Luv.Os_fd.Socket.t = Obj.magic(fd);
        let poll = Luv.Poll.init_socket(~loop, luvSocket) |> Result.get_ok;
        Hashtbl.replace(socketPollHandleTbl, fd, poll);
        poll;
      };
    let events =
      []
      @ (action != M.POLL_IN ? [`WRITABLE] : [])
      @ (action != M.POLL_OUT ? [`READABLE] : []);
    Luv.Poll.start(pollHandle, events, perform(fd));
  | M.POLL_REMOVE =>
    Hashtbl.find_opt(socketPollHandleTbl, fd)
    |> Option.iter(pollHandle => Luv.Poll.stop(pollHandle) |> ignore)
  | _ => ()
  };
};

let onTimeout = () => {
  M.action_timeout(mt);
  checkInfo();
};

let startTimeout = (timer: Luv.Timer.t, timeoutMs: int) =>
  if (timeoutMs < 0) {
    Luv.Timer.stop(timer) |> ignore;
  } else {
    let timeoutMs = timeoutMs == 0 ? 1 : timeoutMs;
    Luv.Timer.start(timer, timeoutMs, onTimeout) |> ignore;
  };

let addUrl = (url: string) => {
  let handle = Curl.init();
  let writeFunc = str => {
    Printf.printf("%s", str);
    String.length(str);
  };
  Curl.set_writefunction(handle, writeFunc);
  Curl.set_url(handle, url);

  M.add(mt, handle);
  Printf.eprintf("Added download: %s", url);
};

let () = {
  Printexc.record_backtrace(true);

  let loop = Luv.Loop.default();
  let timer = Luv.Timer.init(~loop, ()) |> Result.get_ok;
  M.set_socket_function(mt, handleSocket(loop));
  M.set_timer_function(mt, startTimeout(timer));

  let urls = ["https://zachbayl.in"];

  List.iter(addUrl, urls);

  Luv.Loop.run(~loop, ~mode=`DEFAULT, ()) |> ignore;
};
