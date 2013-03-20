/*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

static void
setnonblock(int fd)
{
  int flags;
  flags = fcntl(fd, F_GETFL);
  if (flags == -1)
    perror("fcntl");
  flags |= O_NONBLOCK;
  if (fcntl(fd, F_SETFL, flags) == -1)
    perror("fcntl");
}

#if defined(__linux__)

#include <net/if.h>
#include <sys/ioctl.h>
#include <linux/if_tun.h>

static int
tun_alloc(char *dev, int flags, int persist, int user, int group)
{
  struct ifreq ifr;
  int fd;

  if ((fd = open("/dev/net/tun", O_RDWR)) == -1) {
    perror("open");
    caml_failwith("unable to open /dev/net/tun");
  }

  memset(&ifr, 0, sizeof(ifr));
  ifr.ifr_flags = flags;

  if (*dev)
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);

  if (ioctl(fd, TUNSETIFF, (void *)&ifr) < 0) {
    perror("TUNSETIFF");
    caml_failwith("TUNSETIFF failed");
  }

  if(ioctl(fd, TUNSETPERSIST, persist) < 0) {
    perror("TUNSETPERSIST");
    caml_failwith("TUNSETPERSIST failed");
  }

  if(user != -1) {
    if(ioctl(fd, TUNSETOWNER, user) < 0) {
      perror("TUNSETOWNER");
      caml_failwith("TUNSETOWNER failed");
    }
  }

  if(group != -1) {
    if(ioctl(fd, TUNSETGROUP, group) < 0) {
      perror("TUNSETGROUP");
      caml_failwith("TUNSETGROUP failed");
    }
  }

  strcpy(dev, ifr.ifr_name);
  return fd;
}

CAMLprim value
opendev(value devname, value kind, value pi, value persist, value user, value group)
{
  CAMLparam5(devname, kind, pi, persist, user);
  CAMLxparam1(group);
  CAMLlocal2(res, dev_caml);

  char dev[IFNAMSIZ];
  int fd;
  int flags;

  flags = (Int_val(kind) ? IFF_TUN : IFF_TAP) | (Bool_val(pi) ? 0 : IFF_NO_PI);

  memset(dev, 0, sizeof dev);
  memcpy(dev, String_val(devname), caml_string_length(devname));

  fd = tun_alloc(dev, flags, Bool_val(persist), Int_val(user), Int_val(group));
  setnonblock(fd);

  res = caml_alloc_tuple(2);
  dev_caml = caml_copy_string(dev);

  Store_field(res, 0, Val_int(fd));
  Store_field(res, 1, dev_caml);

  CAMLreturn(res);
}

CAMLprim value
opendev_byte(value *argv, int argn)
{
  return opendev(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value
get_hwaddr(value devname) {
  CAMLparam1(devname);
  CAMLlocal1(hwaddr);

  int fd;
  struct ifreq ifq;

  fd = socket(PF_INET, SOCK_DGRAM, 0);
  strcpy(ifq.ifr_name, String_val(devname));
  if (ioctl(fd, SIOCGIFHWADDR, &ifq) == -1)
    perror("SIOCIFHWADDR");

  hwaddr = caml_alloc_string(6);
  memcpy(String_val(hwaddr), ifq.ifr_hwaddr.sa_data, 6);

  CAMLreturn (hwaddr);
}

#elif defined(__APPLE__) && defined(__MACH__)

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/if_dl.h>
#include <net/ndrv.h>
#include <net/bpf.h>
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <netdb.h>

CAMLprim value
tap_opendev(value v_str)
{
  char name[IFNAMSIZ];
  snprintf(name, sizeof name, "/dev/%s", String_val(v_str));

  int dev_id;

  //small hack to create multiple interfaces
  sscanf(name, "/dev/tap%d", &dev_id);
  fprintf(stderr, "I should be opening 10.%d.0.1\n", dev_id);

  fprintf(stderr, "opendev: %s\n", name);
  int fd = open(name, O_RDWR);
  if (fd < 0)
    err(1, "tap_opendev");
  setnonblock(fd);
  /* Mark interface as up
     Since MacOS doesnt have ethernet bridging built in, the
     IP binding is temporary until someone writes a KPI filter for Darwin */
  char buf[1024];
  snprintf(buf, sizeof buf, "/sbin/ifconfig %s 10.%d.0.1 netmask 255.255.255.0 up", String_val(v_str), dev_id);
  fprintf(stderr, "%s\n", buf);
  system(buf);
  return Val_int(fd);
}

CAMLprim value
get_mac_addr(value v_str) {
  CAMLparam1( v_str );
  CAMLlocal1(v_mac);

  struct ifaddrs *ifap, *p;
  char *mac_addr[6];
  int found = 0;
  char name[IFNAMSIZ];
  snprintf(name, sizeof name, "%s", String_val(v_str));

  if (getifaddrs(&ifap) != 0) {
    err(1, "get_mac_addr");
  }

  for(p = ifap; p != NULL; p = p->ifa_next) {
    if((strcmp(p->ifa_name, name) == 0) &&
      (p->ifa_addr != NULL)){
      char *tmp = LLADDR((struct sockaddr_dl *)(p)->ifa_addr);
      memcpy(mac_addr, tmp, 6);
      found = 1;
      break;
    }
  }

  freeifaddrs(ifap);
  if (!found)
    err(1, "get_mac_addr");

  v_mac = caml_alloc_string(6);
  memcpy(String_val(v_mac), mac_addr, 6);
  CAMLreturn (v_mac);
}

#endif
