# Script Name : DefinePageChecker.py
# Author      : Xianhua Zeng
# Created     : 14th Oct 2021
# Version     : 0.2
# Description : Verify page number hyperlinks in define.xml against an aCRF PDF.
#               Supports both GUI and CLI modes.
#
# CLI usage:
#   DefinePageChecker.py check -d DIR [-o DIR]
#   DefinePageChecker.py check --define PATH --acrf PATH [-o DIR]

import sys
import argparse
import threading
import webbrowser
from datetime import datetime
from getpass import getuser
from os import path, listdir
from re import search, sub
import xml.etree.ElementTree as ET
import PyPDF2


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
APP_NAME    = 'DefinePageChecker'
VERSION_CUR = '0.1'


# ---------------------------------------------------------------------------
# Core checker — no GUI dependency
# ---------------------------------------------------------------------------

class Checker:
    """
    Handles verification of page number hyperlinks in define.xml vs. aCRF PDF.
    Used by both the GUI (Application) and CLI entry point.

    Parameters
    ----------
    log_fn   : callable(str)        — receives progress/info messages
    warn_fn  : callable(str)        — receives orange-level notes
    error_fn : callable(str)        — receives red-level errors
    """

    def __init__(self, log_fn=print, warn_fn=print, error_fn=print):
        self._log   = log_fn
        self._warn  = warn_fn
        self._error = error_fn

    def find_files(self, directory: str):
        """
        Locate define.xml and aCRF PDF in *directory*.
        Returns (define_path, acrf_path) or raises FileNotFoundError.
        """
        files = listdir(directory)

        acrf_list = [f for f in files if 'acrf' in f.lower() and f.endswith('.pdf')]
        if not acrf_list:
            raise FileNotFoundError('Annotated CRF (aCRF PDF) not found in directory.')
        acrf_path = path.join(directory, acrf_list[0])

        define_list = [f for f in files if 'define' in f.lower() and f.endswith('.xml')]
        if not define_list:
            raise FileNotFoundError('Define.xml not found in directory.')
        define_path = path.join(directory, define_list[0])

        return define_path, acrf_path

    def run_check(self, define_path: str, acrf_path: str) -> int:
        """
        Verify page number hyperlinks in *define_path* against *acrf_path*.
        Returns the total number of issues found (0 = clean).
        """
        self._log(f'{APP_NAME} operation is in progress, please wait...')
        start = datetime.now()

        tree   = ET.parse(define_path)
        reader = PyPDF2.PdfReader(open(acrf_path, 'rb'))

        n_pages  = len(reader.pages)
        issue_n  = 0
        page_dict = {}

        # ── Phase 1: validate hyperlink page numbers ───────────────────────
        for node in tree.findall('.//{http://www.cdisc.org/ns/odm/v1.3}ItemDef'):
            domain   = node.attrib['OID'].split('.')[1]
            variable = node.attrib['SASFieldName'].strip()

            if domain[-2:] != variable[:2]:
                variable_ = domain + '.' + variable
            else:
                variable_ = variable

            page_issue = []

            for child in node:
                for grandchild in child.iter():
                    if search(r'PDF', grandchild.tag):
                        page_str  = sub(r',', r' ', grandchild.attrib['PageRefs'])
                        page_list = page_str.split()

                        for page_ref in page_list:
                            xml_page = int(page_ref) - 1

                            try:
                                page_obj = reader.pages[xml_page]
                            except IndexError:
                                issue_n += 1
                                self._error(
                                    f'Error: page ({xml_page + 1}) for {variable} '
                                    f'is greater than total page of aCRF ({n_pages}).'
                                )
                                continue

                            try:
                                found = False
                                for annot in page_obj['/Annots']:
                                    if annot.get_object()['/Subtype'] == '/FreeText':
                                        if search(variable, annot.get_object()['/Contents']):
                                            found = True
                                            break
                                if not found:
                                    issue_n += 1
                                    page_issue.append(str(xml_page + 1))
                                    page_dict[variable_] = page_issue
                            except Exception:
                                pass

        for key, pages in page_dict.items():
            if pages:
                self._error(f'Error: {key} not found on page {", ".join(pages)}.')

        # ── Phase 2: flag pages with no annotations (skip cover pages) ────
        for i in range(n_pages):
            page_obj = reader.pages[i]
            try:
                for annot in page_obj['/Annots']:
                    annot.get_object()['/Subtype']
            except Exception:
                if i > 15:
                    issue_n += 1
                    self._warn(f'Note: there are no annotations on page {i + 1}.')

        # ── Summary ────────────────────────────────────────────────────────
        dur = max((datetime.now() - start).seconds, 1)
        if issue_n == 0:
            self._log('No issue found!')
        self._log(
            f'{APP_NAME} operation is complete. '
            f'Execution time: {dur} second{"s" if dur != 1 else ""}.'
        )
        return issue_n


# ---------------------------------------------------------------------------
# GUI Application
# ---------------------------------------------------------------------------

import tkinter as tk
from tkinter import filedialog, messagebox


class Application(tk.Tk):
    def __init__(self):
        super().__init__()
        self._checker = Checker(
            log_fn   = self.__status,
            warn_fn  = lambda m: self.__status_colored(m, 'tag2', 'orange'),
            error_fn = lambda m: self.__status_colored(m, 'tag1', 'red'),
        )
        self.dirname1 = ''
        self.createWidgets()

    # For ico
    def resource_path(self, relative_path):
        """Get absolute path to resource, works for dev and for PyInstaller."""
        try:
            base_path = sys._MEIPASS
        except Exception:
            base_path = path.abspath('.')
        return path.join(base_path, relative_path)

    def createWidgets(self):
        screen_width  = self.winfo_screenwidth()
        screen_height = self.winfo_screenheight()
        window_width  = 580
        window_height = 370
        pos_top   = int(screen_height / 2 - window_height / 2)
        pos_right = int(screen_width  / 2 - window_width  / 2)

        self.title(f'{APP_NAME} version {VERSION_CUR}')
        self.iconbitmap(default=self.resource_path(APP_NAME + '.ico'))
        self.geometry(f'{window_width}x{window_height}+{pos_right}+{pos_top}')
        self.resizable(0, 0)

        self.entryvar1 = tk.StringVar()

        topframe    = tk.Frame(self, height=10)
        topframer   = tk.Frame(self)
        contentframe = tk.Frame(self)
        topframe.pack(side=tk.TOP)
        topframer.pack(side=tk.TOP)
        contentframe.pack(side=tk.TOP)

        glabel  = tk.Label(topframe, text='Define/aCRF Path:', width=15, font=('Arial', 10))
        gentry  = tk.Entry(topframe, textvariable=self.entryvar1, width=50, font=('Arial', 10))
        gbutton = tk.Button(topframe, bg='CornflowerBlue', command=self.__opendir,
                            text='Browse...', width=8, font=('Arial', 10))
        gbutton3 = tk.Button(topframe, bg='CornflowerBlue', command=self.__go,
                             text='Check', width=8, font=('Arial', 10))
        glabel3  = tk.Label(topframer,
                            text='Status:' + ' ' * 135,
                            font=('Arial', 10), justify='left')

        tk.Label(topframe, text='').grid(row=0, column=0)
        glabel.grid(row=2,  column=0, sticky=tk.E)
        gentry.grid(row=2,  column=1)
        tk.Label(topframe, text='', width=0).grid(row=2, column=2)
        gbutton.grid(row=2,  column=3)
        gbutton3.grid(row=4, column=1)
        glabel3.grid(row=5,  column=0, sticky=tk.E)

        xscrollbar    = tk.Scrollbar(contentframe, orient=tk.HORIZONTAL)
        rightbar      = tk.Scrollbar(contentframe, orient=tk.VERTICAL)
        self.textbox  = tk.Text(contentframe,
                                xscrollcommand=xscrollbar.set,
                                yscrollcommand=rightbar.set,
                                wrap='none',
                                width=80, height=15, font=('Arial', 10))
        xscrollbar.pack(side=tk.BOTTOM, fill=tk.X)
        rightbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.textbox.pack(side=tk.LEFT, fill=tk.BOTH)
        xscrollbar.config(command=self.textbox.xview)
        rightbar.config(command=self.textbox.yview)

    def __opendir(self):
        self.dirname1 = filedialog.askdirectory()
        self.entryvar1.set(self.dirname1)
        if not self.dirname1:
            messagebox.showwarning('Note', message='No folder selected!')

    def __status(self, msg: str):
        self.textbox.insert(tk.END, msg + '\n')
        self.textbox.see(tk.END)
        self.textbox.update()

    def __status_colored(self, msg: str, tag: str, color: str):
        self.textbox.insert(tk.END, msg + '\n', tag)
        self.textbox.tag_config(tag, foreground=color)
        self.textbox.see(tk.END)
        self.textbox.update()

    def _go_thread(self):
        self.textbox.delete('1.0', tk.END)
        if not self.dirname1:
            messagebox.showwarning('Note', message='No folder selected!')
            return

        try:
            define_path, acrf_path = self._checker.find_files(self.dirname1)
        except FileNotFoundError as exc:
            messagebox.showwarning('Error', message=str(exc))
            self.__status(f'{APP_NAME} operation is aborted.')
            return

        self._checker.run_check(define_path, acrf_path)

    def __go(self):
        threading.Thread(target=self._go_thread, daemon=True).start()

    def addmenu(self, menu_cls):
        menu_cls(self)


# ---------------------------------------------------------------------------
# Menu
# ---------------------------------------------------------------------------

class Menu:
    def __init__(self, root: tk.Tk):
        self.root = root
        menubar = tk.Menu(root)

        file_menu = tk.Menu(menubar, tearoff=0)
        file_menu.add_command(label='Exit', command=root.quit)

        help_menu = tk.Menu(menubar, tearoff=0)
        help_menu.add_command(label='Report Issue', command=self._open_issue)
        help_menu.add_command(label='About',        command=self._show_about)

        menubar.add_cascade(label='File', menu=file_menu)
        menubar.add_cascade(label='Help', menu=help_menu)
        root.config(menu=menubar)

    def _open_issue(self):
        webbrowser.open('https://github.com/XianhuaZeng/PharmaSUG/issues/new')

    def _show_about(self):
        win = tk.Toplevel(self.root)
        win.withdraw()
        win.title('About')
        win.resizable(False, False)

        frame = tk.Frame(win)
        frame.pack(padx=30, pady=20)

        tk.Label(frame, text=f'{APP_NAME} version {VERSION_CUR}',
                 font=('Arial', 10, 'bold')).pack(pady=2)
        tk.Label(frame, text='Xianhua Zeng').pack(pady=2)

        def make_link(parent, text, url):
            lbl = tk.Label(parent, text=text, fg='blue', cursor='hand2',
                           font=('Arial', 9, 'underline'))
            lbl.pack(pady=2)
            lbl.bind('<Button-1>', lambda _: webbrowser.open(url))
            lbl.bind('<Enter>',    lambda _: lbl.config(fg='purple'))
            lbl.bind('<Leave>',    lambda _: lbl.config(fg='blue'))

        make_link(frame, 'huazizeng@gmail.com', 'mailto:huazizeng@gmail.com')
        make_link(frame, 'www.xianhuazeng.com', 'https://www.xianhuazeng.com')

        btn_frame = tk.Frame(win)
        btn_frame.pack(pady=(10, 20))
        tk.Button(btn_frame, text='OK', width=10, command=win.destroy).pack()

        win.update_idletasks()
        w, h = win.winfo_width(), win.winfo_height()
        x = win.winfo_screenwidth()  // 2 - w // 2
        y = win.winfo_screenheight() // 2 - h // 2
        win.geometry(f'+{x}+{y}')
        win.after(10, lambda: (win.deiconify(), win.attributes('-alpha', 1.0)))
        win.transient(self.root)
        win.grab_set()


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def build_cli_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog=APP_NAME,
        description='Verify page number hyperlinks in define.xml against an aCRF PDF.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
examples:
  # Auto-detect define.xml and aCRF from a folder
  DefinePageChecker.py check -d C:\\submission

  # Specify files explicitly
  DefinePageChecker.py check --define C:\\sub\\define.xml --acrf C:\\sub\\acrf.pdf
        """,
    )
    parser.add_argument('--version', action='version',
                        version=f'{APP_NAME} {VERSION_CUR}')

    sub = parser.add_subparsers(dest='command', metavar='COMMAND')

    p_check = sub.add_parser(
        'check',
        help='Verify page number hyperlinks in define.xml against an aCRF PDF',
    )

    # Source: either a directory (auto-detect) or explicit file paths
    src = p_check.add_mutually_exclusive_group(required=True)
    src.add_argument('-d', '--dir', metavar='DIR',
                     help='Directory containing define.xml and the aCRF PDF '
                          '(files are auto-detected by name)')
    src.add_argument('--define', metavar='FILE',
                     help='Path to define.xml (use together with --acrf)')

    p_check.add_argument('--acrf', metavar='FILE',
                         help='Path to the annotated CRF PDF '
                              '(required when --define is used)')

    return parser


def run_cli(args):
    issues_found = [0]   # mutable container so lambdas can write to it

    def log_fn(msg):
        print(msg)

    def warn_fn(msg):
        print(f'  {msg}')

    def error_fn(msg):
        print(f'  {msg}', file=sys.stderr)

    checker = Checker(log_fn=log_fn, warn_fn=warn_fn, error_fn=error_fn)

    if args.command == 'check':
        if args.dir:
            directory = path.abspath(args.dir)
            if not path.isdir(directory):
                print(f'ERROR: Directory not found: {directory}')
                sys.exit(1)
            try:
                define_path, acrf_path = checker.find_files(directory)
            except FileNotFoundError as exc:
                print(f'ERROR: {exc}')
                sys.exit(1)
        else:
            if not args.acrf:
                print('ERROR: --acrf is required when --define is used.')
                sys.exit(1)
            define_path = path.abspath(args.define)
            acrf_path   = path.abspath(args.acrf)
            for fp in (define_path, acrf_path):
                if not path.isfile(fp):
                    print(f'ERROR: File not found: {fp}')
                    sys.exit(1)

        issue_count = checker.run_check(define_path, acrf_path)
        sys.exit(0 if issue_count == 0 else 1)


# ---------------------------------------------------------------------------
# Entry point — CLI if arguments given, GUI otherwise
# ---------------------------------------------------------------------------

if __name__ == '__main__':
    if len(sys.argv) > 1:
        cli_parser = build_cli_parser()
        cli_args   = cli_parser.parse_args()
        if not cli_args.command:
            cli_parser.print_help()
            sys.exit(0)
        run_cli(cli_args)
    else:
        root = Application()
        root.addmenu(Menu)
        root.mainloop()

# End of Code
