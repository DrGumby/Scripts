import curses
import curses.panel
import logging
import time
from curses.textpad import Textbox, rectangle


from db import Database


# Global Database object.
#FIXME Change to non global implementation
database = None


def get_query(query):
    """Performs specified DB query"""
    global database
    res = database.conn.execute(query)
    out = res.fetchall()
    return [dict(zip(i.keys(), i)) for i in out]

def make_windows(stdscr, margins=2):
    """ Creates 4 windows with specified margins """
    # Get screen size
    sh, sw = stdscr.getmaxyx()
    ch = sh//2
    cw = sw//2

    m = margins
    subwindows = []
    # Create 4 rectangles
    boxes = (
        ((m,m),(ch-m,cw-m)),
        ((m,cw+m),(ch-m,sw-m)),
        ((ch+m,m),(sh-m,cw-m)),
        ((ch+m,cw+m),(sh-m,sw-m))
    )
    # Draw rectangles and create windows
    for box in boxes:
        rectangle(stdscr, box[0][0],box[0][1],box[1][0],box[1][1])
        subwindows.append(curses.newwin(ch-m-3, cw-m-3, box[0][0]+1, box[0][1]+1))

    stdscr.refresh()
    return boxes, subwindows


def get_msg(stdscr, prompt, screen_box=((-1,-1),(0,0))):
    """ Create a single line prompt at specified position """
    sh, sw = stdscr.getmaxyx()


    box = [
        (sh//2-1,3),
        (sh//2+1, sw-3)
    ]

    # Add a prompt to given location
    stdscr.addstr(box[0][0]//2, sw//2 - len(prompt)//2, prompt)

    # Create an editable subwindow
    editwin = curses.newwin(1, box[1][1]-4, box[0][0]+screen_box[0][0]+2, box[0][1]+screen_box[0][1]+2)
    rectangle(stdscr, box[0][0], box[0][1], box[1][0], box[1][1])
    stdscr.refresh()

    # Make window editable
    box = Textbox(editwin, True)
    curses.curs_set(1)
    # Let the user edit until Ctrl-G is struck.
    box.edit()
    curses.curs_set(0)
    # Get resulting contents
    return(box.gather())


def print_menu(stdscr, selected_row, menu):
    """ Creates a menu selection """
    stdscr.clear()
    h, w = stdscr.getmaxyx()

    endpositions = []

    # Show centered menu items
    for idx, row in enumerate(menu):
        x = w//2 - len(row)//2
        y = h//2 - len(menu)//2 + idx
        if idx == selected_row:
            stdscr.attron(curses.color_pair(1))
            stdscr.addstr(y, x, row)
            stdscr.attroff(curses.color_pair(1))
        else:
            stdscr.addstr(y, x, row)
        endpositions.append(tuple((y, x+len(row))))

    stdscr.refresh()
    return endpositions


def make_box(stdscr, margin:int):
    """ Create a box within the window """
    sh, sw = stdscr.getmaxyx()

    box = (
        (margin, margin),
        (sh-margin, sw-margin)
    )
    rectangle(stdscr, box[0][0], box[0][1], box[1][0], box[1][1])
    stdscr.refresh()
    return box

def make_window(box, margin):
    return curses.newwin(box[1][0]-box[0][0]-margin, box[1][1]-box[0][1]-margin, box[0][0]+1, box[0][1]+1)


def add_multiline_string_to_center(stdscr, lines):
    """ Parses a list of string and shows them centered in specified window """
    h, w = stdscr.getmaxyx()

    for idx, row in enumerate(lines):
        x = w//2 - len(row)//2
        y = h//2 - len(lines)//2 + idx
        stdscr.addstr(y, x, row)
    stdscr.refresh()

def show_help(stdscr):
    """ Show title screen help message """
    stdscr.clear()
    box = make_box(stdscr, 6)
    win = make_window(box, 1)


    msg = ["Welcome to DND interactive manager.", "", "Use the arrow keys to navigate.", "To use, the program will first ask you to connect to a database.", "If the database does not exist, it will be created.", "Press enter or q to go back"]
    add_multiline_string_to_center(win, msg)

    stdscr.refresh()
    while True:
        key = stdscr.getch()
        if key == curses.KEY_ENTER or key in [10, 13, ord('q')]:
            return

def add_table_centered(stdscr, table):
    """ Shows a table centered in the spewcified window """
    keys = [str(i[0]) for i in table]
    values = [str(i[1]) for i in table]
    maxlen_key = max(map(len, keys))
    maxlen_val = max(map(len, values))
    lines = []
    for i in table:
        spaces = (maxlen_key - len(str(i[0]))) + (maxlen_val - len(str(i[1])))
        line = str(i[0]).upper() + (' ' * (spaces + 2)) + str(i[1])
        lines.append(line)

    add_multiline_string_to_center(stdscr, lines)

def create_interactive_table(stdscr, keys):

    current_row = 0

    values = ['']*len(keys)
    subwindows = []
    textboxes = []
    positions = print_menu(stdscr, current_row, keys)
    for i in range(len(keys)):
        subwindows.append(curses.newwin(1, 15, positions[i][0], positions[i][1]+2))
    for idx, window in enumerate(subwindows):
        window.addstr(0, 0, values[idx])
        window.refresh()
        textboxes.append(Textbox(window))

    while True:
        key = stdscr.getch()
        stdscr.clear()

        if key == curses.KEY_ENTER or key in [10, 13]:
            curses.curs_set(1)
            textboxes[current_row].edit()
            curses.curs_set(0)
            values[current_row] = textboxes[current_row].gather()
        elif key == curses.KEY_UP and current_row > 0:
            current_row -= 1
        elif key == curses.KEY_DOWN and current_row < len(keys)-1:
            current_row += 1

        positions = print_menu(stdscr, current_row, keys)
        for idx, window in enumerate(subwindows):
            window.addstr(0, 0, values[idx])
            window.refresh()

        stdscr.refresh()



def create_player(stdscr):
    global database
    create_interactive_table(stdscr, ['TEst', 'test2'])


def select_player(stdscr):
    """ Create a player selection screen """
    global database
    stdscr.clear()
    box = make_box(stdscr, 6)
    win = make_window(box, 1)

    sh, sw = stdscr.getmaxyx()
    msg = "Please select active player:"
    msg_help = "Enter to select, q to exit"
    players = get_query(database.Tables.players.select())

    current_row = 0
    menu = [str(i['id']) + ' ' + i['name'] for i in players]
    menu.append('New player')

    print_menu(win, current_row, menu)
    stdscr.addstr(4, sw//2-len(msg)//2, msg)
    stdscr.addstr(5, sw//2-len(msg_help)//2, msg_help)
    stdscr.refresh()
    while True:
        key = stdscr.getch()
        logging.debug(key)
        win.clear()

        if key == curses.KEY_UP and current_row > 0:
            current_row -= 1
        elif key == curses.KEY_DOWN and current_row < len(menu)-1:
            current_row += 1
        elif key == ord('q'):
            return True
        elif key == curses.KEY_ENTER or key in [10, 13]:
            logging.debug(menu[current_row])
            if menu[current_row] == 'New player':
                create_player(stdscr)
            else:
                return handle_game(stdscr, players[current_row])

        print_menu(win, current_row, menu)
        win.refresh()


def reload_player(id):
    """ Performs a database query and reloads specified player """
    global database
    table = database.Tables.players
    return get_query(table.select().where(table.c.id == id))


def ammo(player, arg):
    """ Changes ammo value in database """
    global database
    table = database.Tables.players
    upd = table.update(None).where(table.c.id == player['id']).values(ammo=table.c.ammo+(int(arg)))
    database.conn.execute(upd)

def hp(player, arg):
    """ Changes hp value in databaser """
    global database
    table = database.Tables.players
    upd = table.update(None).where(table.c.id == player['id']).values(hp=table.c.hp+(int(arg)))
    database.conn.execute(upd)

def money(player, arg):
    """ Changes money value in database """
    global database
    table = database.Tables.players
    upd = table.update(None).where(table.c.id == player['id']).values(money=table.c.money+(int(arg)))
    database.conn.execute(upd)

def handle_game(stdscr, player):
    """ Handles current game """
    global database
    stdscr.clear()

    # Create 4 subwindows
    boxes, subwindows = make_windows(stdscr)

    # Create menu list
    player_action_menu = ['Shoot', 'Craft ammo', 'Take damage', 'Heal damage', 'Spend money', 'Earn money']
    current_row = 0

    # Add player information table in top left window
    add_table_centered(subwindows[0], list(player.items()))

    # Show a menu
    print_menu(subwindows[1], current_row, player_action_menu)

    while True:
        # Read a key
        key = stdscr.getch()
        # Clear the screen
        subwindows[1].clear()

        # Move armound the menu
        if key == curses.KEY_UP and current_row > 0:
            current_row -= 1
        elif key == curses.KEY_DOWN and current_row < len(player_action_menu)-1:
            current_row += 1
        elif key == ord('q'):
            return True
        # Perform selected action
        elif key == curses.KEY_ENTER or key in [10, 13, ord('+')]:
            cnt = 1
            if key == ord('+'):
                cnt = int(get_msg(subwindows[3], "How many", boxes[3])) or 0
                subwindows[3].clear()
            if player_action_menu[current_row] == 'Shoot':
                ammo(player, -cnt)
            elif player_action_menu[current_row] == 'Craft ammo':
                ammo(player, +cnt)
            elif player_action_menu[current_row] == 'Take damage':
                hp(player, -cnt)
            elif player_action_menu[current_row] == 'Heal damage':
                hp(player, +cnt)
            elif player_action_menu[current_row] == 'Spend money':
                money(player, -cnt)
            elif player_action_menu[current_row] == 'Earn money':
                money(player, +cnt)

            # Reload player after changing values
            player = reload_player(player['id'])[0]
            logging.debug(player)

        # Reload menu and windows
        print_menu(subwindows[1], current_row, player_action_menu)
        add_table_centered(subwindows[0], list(player.items()))
        for i in subwindows:
            i.refresh()


def connect_db(stdscr):
    """ Connect to database using a prompt """
    global database
    msg = get_msg(stdscr, "Enter database name. Exit with ENTER or C-g")
    logging.debug(msg)
    database = Database(msg.strip())
    logging.debug(database.engine.table_names())
    return select_player(stdscr)

def start_menu(stdscr):
    """ Show start menu """
    curses.init_pair(1, curses.COLOR_BLACK, curses.COLOR_WHITE)
    menu = ['Start', 'Help', 'Exit']
    current_row = 0

    print_menu(stdscr, current_row, menu)

    while True:
        key = stdscr.getch()
        stdscr.clear()

        if key == curses.KEY_UP and current_row > 0:
            current_row -= 1
        elif key == curses.KEY_DOWN and current_row < len(menu)-1:
            current_row += 1
        elif key == curses.KEY_ENTER or key in [10, 13]:
            if menu[current_row] == 'Exit':
                return True
            elif menu[current_row] == 'Help':
                show_help(stdscr)
            elif menu[current_row] == 'Start':
                connect_db(stdscr)

        print_menu(stdscr, current_row, menu)
        stdscr.refresh()

def main(stdscr):
    curses.curs_set(0)
    sh, sw = stdscr.getmaxyx()
    if sh < 25 or sw < 80:
        return False
    return start_menu(stdscr)

# Select log file
logging.basicConfig(filename='out.log', level=logging.DEBUG)
curses.wrapper(main)
