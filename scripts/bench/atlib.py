import os, fcntl, struct, termios, time

TIOCMGET = 0x5415

class AT:
    def __init__(self, port='/dev/ttyACM0'):
        self.fd = os.open(port, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
        a = termios.tcgetattr(self.fd)
        a[0]=a[1]=a[3]=0
        a[2] = termios.CS8 | termios.CREAD | termios.CLOCAL
        a[4]=a[5]=termios.B115200
        a[6][termios.VMIN]=0; a[6][termios.VTIME]=0
        termios.tcsetattr(self.fd, termios.TCSANOW, a)
        self.drain(0.3)
    def drain(self, t=0.3):
        out=b''; end=time.time()+t
        while time.time()<end:
            try: out += os.read(self.fd, 65536)
            except BlockingIOError: time.sleep(0.01)
        return out
    def cmd(self, c, wait=3.0, stop=None, quiet=False):
        self.drain(0.15)
        os.write(self.fd, (c+'\r').encode())
        out=b''; end=time.time()+wait
        while time.time()<end:
            try: out += os.read(self.fd, 65536)
            except BlockingIOError: time.sleep(0.01)
            if stop and any(s in out for s in stop): break
            if not stop and (b'OK\r\n' in out or b'ERROR\r\n' in out): break
        if not quiet:
            txt = out.decode('latin1').replace('\r','\n')
            txt = ' | '.join(l for l in txt.split('\n') if l.strip())
            print(f'  {c:<22} -> {txt}')
        return out
    def close(self):
        os.close(self.fd)
