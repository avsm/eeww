/*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Vincent Bernardoff <vb@luminar.eu.org>
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
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <sys/types.h>
#include <sys/socket.h>
#if defined(__FreeBSD__) || defined(__OpenBSD__)
#include <netinet/in.h>
#endif /* __FreeBSD__ */
#include <ifaddrs.h>
#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

static void
tun_raise_error(char *prefix, int fd)
{
  char buf[1024];
  snprintf(buf, sizeof(buf)-1, "tun[%s]: %s", prefix, strerror(errno));
  buf[sizeof(buf)-1] = '\0';
  if (fd >= 0) close(fd);
  caml_failwith(buf);
}

#if defined(__linux__)
#include <linux/if_tun.h>

static int
tun_alloc(char *dev, int kind, int pi, int persist, int user, int group)
{
  struct ifreq ifr;
  int fd;

  if ((fd = open("/dev/net/tun", O_RDWR | O_NONBLOCK)) == -1)
    tun_raise_error("open", -1);

  memset(&ifr, 0, sizeof(ifr));
  ifr.ifr_flags = 0;
  ifr.ifr_flags |= (kind ? IFF_TUN : IFF_TAP);
  ifr.ifr_flags |= (pi ? 0 : IFF_NO_PI);

  if (*dev)
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);

  if (ioctl(fd, TUNSETIFF, (void *)&ifr) < 0)
    tun_raise_error("TUNSETIFF", fd);

  if (persist != -1) {
    if (ioctl(fd, TUNSETPERSIST, persist) < 0)
      tun_raise_error("TUNSETPERSIST", fd);
  }

  if (user != -1) {
    if(ioctl(fd, TUNSETOWNER, user) < 0)
      tun_raise_error("TUNSETOWNER", fd);
  }

  if (group != -1) {
    if(ioctl(fd, TUNSETGROUP, group) < 0)
      tun_raise_error("TUNSETGROUP", fd);
  }

  strncpy(dev, ifr.ifr_name, IFNAMSIZ);
  return fd;
}

#elif (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__) || defined(__OpenBSD__)
#include <net/if_dl.h>
#include <ifaddrs.h>

static int
tun_alloc(char *dev, int kind, int pi, int persist, int user, int group)
{
  // On MacOSX, we need that dev is not NULL, and all options other
  // than kind are to be ignored because not supported.
  char name[IFNAMSIZ];
  int fd;

  snprintf(name, sizeof name, "/dev/%s", String_val(dev));

  fd = open(name, O_RDWR);
  if (fd == -1)
    tun_raise_error("open", -1);
  return fd;
}
#endif

#if defined(__linux__) || defined(__FreeBSD__) || defined(__OpenBSD__)
CAMLprim value
get_macaddr(value devname)
{
  CAMLparam1(devname);
  CAMLlocal1(hwaddr);

  int fd;
  struct ifreq ifq;

  fd = socket(AF_LOCAL, SOCK_DGRAM, 0);
  strncpy(ifq.ifr_name, String_val(devname), IFNAMSIZ);
#if defined(__FreeBSD__) || defined(__OpenBSD__)
  ifq.ifr_addr.sa_len = 6;
#endif /* __FreeBSD__ */

#if defined(__linux__)
  if (ioctl(fd, SIOCGIFHWADDR, &ifq) == -1)
    tun_raise_error("SIOCGIFHWADDR", fd);
#elif defined(__FreeBSD__)
  if (ioctl(fd, SIOCGHWADDR, &ifq) == -1)
    tun_raise_error("SIOCGHWADDR", fd);
#else
  if (ioctl(fd, SIOCGIFADDR, &ifq) == -1)
    tun_raise_error("SIOCGIFADDR", fd);
#endif

  close(fd);

  hwaddr = caml_alloc_string(6);
  memcpy(String_val(hwaddr), ifq.ifr_addr.sa_data, 6);

  CAMLreturn (hwaddr);
}

#elif (defined(__APPLE__) && defined(__MACH__))

CAMLprim value
get_macaddr(value devname)
{
  CAMLparam1(devname);
  CAMLlocal1(v_mac);

  struct ifaddrs *ifap, *p;
  char *mac_addr[6];
  int found = 0;
  char name[IFNAMSIZ];
  snprintf(name, sizeof name, "%s", String_val(devname));

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

// Code for all architectures

CAMLprim value
get_mtu(value devname)
{
  CAMLparam1(devname);
  CAMLlocal1(mtu);

  int fd;
  struct ifreq ifq;

  fd = socket(AF_INET, SOCK_DGRAM, 0);
  strncpy(ifq.ifr_name, String_val(devname), IFNAMSIZ);

  if (ioctl(fd, SIOCGIFMTU, &ifq) == -1)
    tun_raise_error("SIOCGIFMTU", fd);

  close(fd);

  CAMLreturn (Val_int(ifq.ifr_mtu));
}

CAMLprim value
set_up_and_running(value dev)
{
  CAMLparam1(dev);

  int fd;
  struct ifreq ifr;
  int flags;

  if((fd = socket(PF_INET, SOCK_DGRAM, 0)) == -1)
    tun_raise_error("set_up_and_running socket", -1);

  strncpy(ifr.ifr_name, String_val(dev), IFNAMSIZ);
  ifr.ifr_addr.sa_family = AF_INET;
#if defined(__FreeBSD__) || defined(__OpenBSD__)
  ifr.ifr_addr.sa_len = IFNAMSIZ;
#endif /* __FreeBSD__ */

  if (ioctl(fd, SIOCGIFFLAGS, &ifr) == -1)
    tun_raise_error("set_up_and_running SIOCGIFFLAGS", -1);

  strncpy(ifr.ifr_name, String_val(dev), IFNAMSIZ);

  flags = ifr.ifr_flags | (IFF_UP|IFF_RUNNING|IFF_BROADCAST|IFF_MULTICAST);
  if (flags != ifr.ifr_flags) {
    ifr.ifr_flags = flags;
    if (ioctl(fd, SIOCSIFFLAGS, &ifr) == -1)
      tun_raise_error("set_up_and_running SIOCSIFFLAGS", -1);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value
set_ipv4(value dev, value ipv4, value netmask)
{
  CAMLparam3(dev, ipv4, netmask);

  int fd;
  struct ifreq ifr;
  struct sockaddr_in* addr = (struct sockaddr_in*)&ifr.ifr_addr;

  memset(&ifr, 0, sizeof(struct ifreq));

  if((fd = socket(PF_INET, SOCK_DGRAM, 0)) == -1)
    tun_raise_error("set_ipv4 socket", -1);

  strncpy(ifr.ifr_name, String_val(dev), IFNAMSIZ);
  ifr.ifr_addr.sa_family = AF_INET;
#if defined(__FreeBSD__) || defined(__OpenBSD__)
  ifr.ifr_addr.sa_len = IFNAMSIZ;
#endif /* __FreeBSD */

  memcpy(&(addr->sin_addr), String_val(ipv4), 4);

  if (ioctl(fd, SIOCSIFADDR, &ifr) == -1)
    tun_raise_error("SIOCSIFADDR", -1);

  if(caml_string_length(netmask) > 0)
    {
      memcpy(&(addr->sin_addr), String_val(netmask), 4);

      if (ioctl(fd, SIOCSIFNETMASK, &ifr) == -1)
        tun_raise_error("SIOCSIFNETMASK", -1);
    }

  // Set interface up and running
  set_up_and_running(dev);

  CAMLreturn(Val_unit);
}

CAMLprim value
tun_opendev(value devname, value kind, value pi, value persist, value user, value group)
{
  CAMLparam5(devname, kind, pi, persist, user);
  CAMLxparam1(group);
  CAMLlocal2(res, dev_caml);

  char dev[IFNAMSIZ];
  int fd;

#if defined (__APPLE__) && defined (__MACH__)
  if (caml_string_length(devname) < 4)
    caml_failwith("On MacOSX, you need to specify the name of the device, e.g. tap0");
#endif

  memset(dev, 0, sizeof dev);
  memcpy(dev, String_val(devname), caml_string_length(devname));

  // All errors are already checked by tun_alloc, returned fd is valid
  // otherwise it would have crashed before
  fd = tun_alloc(dev, Int_val(kind), Bool_val(pi), Int_val(persist), Int_val(user), Int_val(group));

  res = caml_alloc_tuple(2);
  dev_caml = caml_copy_string(dev);

  Store_field(res, 0, Val_int(fd));
  Store_field(res, 1, dev_caml);

  CAMLreturn(res);
}

CAMLprim value
tun_opendev_byte(value *argv, int argn)
{
  return tun_opendev(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value
get_ifnamsiz()
{
  CAMLparam0();
  CAMLreturn(Val_int(IFNAMSIZ));
}

CAMLprim value
getifaddrs_stub()
{
  CAMLparam0();
  CAMLlocal1(opt);

  int ret;
  struct ifaddrs *ifap = NULL;
  ret = getifaddrs(&ifap);
  if (ret == -1)
    tun_raise_error("getifaddrs", -1);

  if (ifap == NULL)
    CAMLreturn(Val_int(0));
  else
    {
      opt = caml_alloc(1, 0);
      Store_field(opt, 0, (value)ifap);
      CAMLreturn(opt);
    }
}

CAMLprim value
freeifaddrs_stub(value ifap)
{
  freeifaddrs((struct ifaddrs *)ifap);
  return Val_unit;
}

CAMLprim value caml_string_of_sa(struct sockaddr *sa)
{
  CAMLparam0();
  CAMLlocal1(ret);

  struct sockaddr_in *sa_in;
  struct sockaddr_in6 *sa_in6;

  switch(sa->sa_family)
    {
    case AF_INET:
      sa_in = (struct sockaddr_in *)sa;
      ret = caml_alloc_string(4);
      memcpy(String_val(ret), &sa_in->sin_addr.s_addr, 4);
      break;

    case AF_INET6:
      sa_in6 = (struct sockaddr_in6 *)sa;
      ret = caml_alloc_string(16);
      memcpy(String_val(ret), sa_in6->sin6_addr.s6_addr, 16);
      break;

    default:
      caml_invalid_argument("caml_string_of_sa: sa_family not supported");
    }

  CAMLreturn(ret);
}

CAMLprim value
iface_get(value ifap)
{
  CAMLparam0();
  CAMLlocal4(ret, opt1, opt2, opt3);

  struct ifaddrs *c_ifap = (struct ifaddrs *)ifap;

  ret = caml_alloc(5, 0);

  Store_field(ret, 0, caml_copy_string(c_ifap->ifa_name));
  Store_field(ret, 1, Val_int(2));
  Store_field(ret, 2, Val_int(0));
  Store_field(ret, 3, Val_int(0));
  Store_field(ret, 4, Val_int(0));

  if (c_ifap->ifa_addr == NULL)
    CAMLreturn(ret);

  switch (c_ifap->ifa_addr->sa_family)
    {
    case AF_INET:
      Store_field(ret, 1, Val_int(0));
      break;
    case AF_INET6:
      Store_field(ret, 1, Val_int(1));
      break;
    }

  if (c_ifap->ifa_addr->sa_family != AF_INET &&
      c_ifap->ifa_addr->sa_family != AF_INET6)
    CAMLreturn(ret);

  opt1 = caml_alloc(1, 0);
  Store_field(opt1, 0, caml_string_of_sa(c_ifap->ifa_addr));
  Store_field(ret, 2, opt1);

  if (c_ifap->ifa_netmask != NULL)
    {
      opt2 = caml_alloc(1, 0);
      Store_field(opt2, 0, caml_string_of_sa(c_ifap->ifa_netmask));
      Store_field(ret, 3, opt2);
    }

  // We want to extract the third sockaddr, be it broadcast or dst.
#if defined (__linux__)
  struct sockaddr *broadaddr = c_ifap->ifa_ifu.ifu_broadaddr;
#else
  struct sockaddr *broadaddr = c_ifap->ifa_dstaddr;
#endif

  if (broadaddr != NULL &&
      /* XXX Work around *BSD bug. Github Pull Request #14 */
      (broadaddr->sa_family == AF_INET ||
       broadaddr->sa_family == AF_INET6))
    {
      opt3 = caml_alloc(1, 0);
      Store_field(opt3, 0, caml_string_of_sa(broadaddr));
      Store_field(ret, 4, opt3);
    }

  CAMLreturn(ret);
}

CAMLprim value
iface_next(value ifap)
{
  CAMLparam0();
  CAMLlocal1(caml_next);
  value ret;

  struct ifaddrs *next = ((struct ifaddrs *)ifap)->ifa_next;

  if (next == NULL)
    ret = Val_int(0);
  else
    {
      caml_next = caml_alloc(1, 0);
      Store_field(caml_next, 0, (value)next);
      ret = caml_next;
    }

  CAMLreturn(ret);
}


