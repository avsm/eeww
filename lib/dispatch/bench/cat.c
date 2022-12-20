#include <dispatch/dispatch.h>
#include <stdio.h>
// Pure GCD cat-like program (used to test the overhead of OCaml's runtime)

dispatch_group_t cat(dispatch_fd_t fd) {
  dispatch_group_t group = dispatch_group_create();
  dispatch_queue_t queue = dispatch_queue_create("ocaml.rw.queue", DISPATCH_QUEUE_SERIAL);
  dispatch_io_t in_io = dispatch_io_create(DISPATCH_IO_STREAM, fd, queue, ^(int err){ return; });
  dispatch_io_t out_io = dispatch_io_create(DISPATCH_IO_STREAM, STDOUT_FILENO, queue, ^(int err){ return; });
  dispatch_io_set_high_water(in_io, 1024 * 64);
  dispatch_group_enter(group);
  dispatch_io_read(in_io, 0, SIZE_MAX, queue,
    ^(bool done, dispatch_data_t data, int error) {
      size_t data_size;
      if (error) {
        printf("Somethings gone wrong!"); 
        return;
      }
      if (done) {
        dispatch_group_leave(group);
      }
      if (data) {
        // data_size = dispatch_data_get_size(data);
        // if (data_size > 0) {
          dispatch_group_enter(group);
          dispatch_io_write(out_io, 0, data, queue,
            ^(bool done, dispatch_data_t data, int error) {
              if (error) {
                printf("Something's gone wrong!");
                return;
              }
              if (done) {
                dispatch_group_leave(group);
              }
            }
          );
        // }
      }
    }
  );

  return group;
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    int fd = open(argv[1], O_RDONLY);
    dispatch_group_wait(cat(fd), DISPATCH_TIME_FOREVER);
  } else {
    dispatch_group_wait(cat(STDIN_FILENO), DISPATCH_TIME_FOREVER);
  }
}