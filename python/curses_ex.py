import time
import curses as c

menu = ['Home', 'Play', 'Scoreboard', 'Exit']

def print_menu(stdscr, selected_row):
    stdscr.clear()
    h, w = stdscr.getmaxyx()

    for idx, row in enumerate(menu):
        x = w//2 - len(row)//2
        y = h//2 - len(menu)//2 + idx
        if idx == selected_row:
            stdscr.attron(c.color_pair(1))
            stdscr.addstr(y, x, row)
            stdscr.attroff(c.color_pair(1))
        else:
            stdscr.addstr(y, x, row)

    stdscr.refresh()

def main(stdscr):
    c.curs_set(0)
    c.init_pair(1, c.COLOR_BLACK, c.COLOR_WHITE)

    current_row = 0

    print_menu(stdscr, current_row)

    while True:
        key = stdscr.getch()
        stdscr.clear()

        if key == c.KEY_UP and current_row > 0:
            current_row -= 1
        elif key == c.KEY_DOWN and current_row < len(menu) - 1:
            current_row += 1
        elif key == c.KEY_ENTER or key in [10, 13]:
            stdscr.clear()
            stdscr.addstr(0,0, "You pressed {}".format(menu[current_row]))
            stdscr.refresh()
            stdscr.getch()

        print_menu(stdscr, current_row)

        stdscr.refresh()

c.wrapper(main)
