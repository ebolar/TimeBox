library(rcurses)

echo <- function() {
  # setup rcurses stuff
  win <- rcurses.initscr()  # initialize curses window
  rcurses.cbreak()  # typed characters submitted right away, no wait for Enter
  rcurses.noecho()  # typed characters are not shown on the screen
  
  rcurses.clear(win)  # arrange setting the screen to all blanks
  rcurses.refresh(win)  # render the changes (i.e. update the screen)
  
  # initialize cursor position to row 0 (top), column 0 (leftmost col)
  y <- 0
  x <- 0
  rcurses.move(win,y,x)
  
  # loop forever waiting for input
  while ((ch <- rcurses.getch(win)) != 'q') {
    paintCharacter(win,ch)  # draw the character
    y <- y + 1  # down one column
    if (y == rcurses.LINES) {  # if past bottom, go to top and right
      y <- 0
      x <- x + 1
      if (x == rcurses.COLS) {  # if past right edge, go to left
        x <- 0
      }
    }
    rcurses.move(win,y,x)  # move cursor to specified row, col
  }
  
  # now restore normal screen status
  rcurses.echo()
  rcurses.nocbreak()
  rcurses.endwin()
}

# draw the specified character ch
paintCharacter <- function(window,ch) {
  rcurses.delch(window)  # delete the character currently there
  rcurses.insch(window,ch)  # insert the new character
  rcurses.refresh(window)  # update the changes on the screen
}

# test usage of echo
testEcho <- function() {
  echo()
}