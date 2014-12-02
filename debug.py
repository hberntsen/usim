#! /usr/bin/env python

import urwid
import urwid.signals
import socket
import sys
import getopt

class Window(urwid.BoxAdapter):
    def __init__(self, body, title = "Unknown title", height = 35):
        #super(Window, self).__init__(
        self.frame = urwid.Frame(
            body,
            header=urwid.AttrMap(
                urwid.Text(('title', title)),
                'title'
            ),
            footer=urwid.AttrMap(
                urwid.Text(('footer', '')),
                'footer'
            )
        )
        super(Window, self).__init__(self.frame, height)

class Command:
    def __init__(self, command = b"show"):
        self.command = command
        self.response = b""
        self.history = []

    def executeCommand(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        global config
        sock.connect(("localhost", config['port']))

        self.send(sock, self.command + "\n")
        self.response = self.receive(sock)

        sock.close()

        self.edit_text = ""

        self.history.append(self.command)

        return self.response

    def send(self, sock, msg):
        if not msg:
            return
        totalsent = 0
        while totalsent < len(msg):
            sent = sock.send(bytes(msg, "UTF-8")[totalsent:])
            if sent == 0:
                raise RuntimeError("Socket connection broken")
            totalsent = totalsent + sent

    def receive(self, sock):
        chunk = sock.recv(4096)
        if chunk == b'':
            raise RuntimeError("Socket connection broken")
        return chunk


class CommandView(Window):
    def __init__(self, command, height = 35):
        self.command = command
        self.txt = urwid.Text("No output")

        self.walker = urwid.SimpleFocusListWalker([self.txt])
        self.box = urwid.ListBox(self.walker)

        super(CommandView, self).__init__(
                self.box,
                "%s" % command,
                height)

        global cli
        urwid.connect_signal(cli, 'executed', self.onExecuted)

    def onExecuted(self):
        cmd = Command(self.command)
        response = cmd.executeCommand()

        self.updateResponse(response)

    def updateResponse(self, raw):
        response = raw.expandtabs(2)
        lines = response.split(b'\n')

        self.box.body[:] = [urwid.Text(line) for line in lines]

    def selectable(self):
        return False

class SourceBrowser(Window):
    def __init__(self):
        lines = []
        self.retreiveFilename()
        with open(self.filename, "r") as f:
            i  = 1
            for line in f:
                lines.append(urwid.Text(u"%4d %s" % (i, line.expandtabs(2).rstrip())))
                i += 1

        walker = urwid.SimpleFocusListWalker(lines)
        self.box = urwid.ListBox(walker)

        global cli
        urwid.connect_signal(cli, 'executed', self.onExecuted)

        super(SourceBrowser, self).__init__(self.box, "file: %s" % self.filename)

        self.onExecuted()

    def retreiveFilename(self):
        cmd = Command("show file")
        resp = cmd.executeCommand()
        self.filename = resp.rstrip();

    def onExecuted(self):
        cmd = Command("show instruction");
        resp = cmd.executeCommand()

        parts = resp.split(b' ')
        if len(parts) > 0:
            linenr = int(parts[0].strip(b'[]'))
            oldLine = self.box.focus
            oldLine.set_text(oldLine.get_text()[0])

            self.box.focus_position = linenr - 1
            line = self.box.focus
            self.box.focus.set_text(('currentLine', line.get_text()[0]))

class CliOutput(CommandView):
    def __init__(self):
        super(CliOutput, self).__init__("")

        self.frame.header.original_widget.set_text('Output: %s' % self.command)
        #self.header.original_widget.set_text('Command output')

    def onExecuted(self):
        global cli
        self.updateResponse(cli.command.response)
        self.frame.header.original_widget.set_text('Output: %s' % cli.command.command)

class Info(urwid.Columns):
    def __init__(self):
        super(Info, self).__init__([], 2)

class CliEdit(urwid.Edit):
    __metaclass__  = urwid.signals.MetaSignals
    signals = ["executed"]

    def __init__(self):
        super(CliEdit, self).__init__(('ps1', '>>> '))


    def keypress(self, size, key):
        if key != 'enter':
            return super(CliEdit, self).keypress(size, key)
        self.command = Command(self.edit_text)
        self.command.executeCommand()
        self.edit_text = ""

        urwid.emit_signal(self, "executed")


class Root(urwid.Frame):
    def __init__(self):
        global cli

        self.info = Info()
        self.browser = SourceBrowser();
        self.info.contents = [
                (CommandView("show registers"), self.info.options('weight', 1)),
                (self.browser, self.info.options('weight', 2)),
                (CommandView("show stack"), self.info.options('weight', 1)),
        ]

        self.cliOutput = CliOutput()
        self.infoBottom = Info()
        self.infoBottom.contents = [
                (CommandView("show"), self.info.options('weight', 1)),
                (self.cliOutput, self.info.options('weight', 2)),
                (CommandView("show data 0x0200"), self.info.options('weight', 1)),
        ]

        self.pile = urwid.Pile([self.info, self.infoBottom])
        self.filler = urwid.Filler(self.pile, "top")

        self.footer = urwid.Pile([urwid.Divider('-'), cli]);

        super(Root, self).__init__(self.filler, footer = self.footer, focus_part='footer')

    def keypress(self, size, key):
        trans = {'page up': 'up', 'page down': 'down' }
        if key in trans:
            return self.cliOutput.box.keypress(size, trans[key])
        return super(Root, self).keypress(size, key)

cli = CliEdit()
config = {
    'port': 3742,
}

def main():
    args = sys.argv[1:]
    try:
        (optlist, args) = getopt.getopt(args, 'p:', ['port='])
    except getopt.GetoptError as e:
        print(e)
        sys.exit(2)

    global config
    for o,a in optlist:
        if o in ['-p', '--port']:
            if a:
                config['port'] = int(a)


    root = Root()
    cli = CliEdit()

    palette = [
        ('title', 'black', 'light gray'),
        ('footer', '', 'dark gray'),
        ('currentLine', 'black', 'light gray'),
    ]

    loop = urwid.MainLoop(root, palette)
    loop.run()

if __name__ == "__main__":
    main();
