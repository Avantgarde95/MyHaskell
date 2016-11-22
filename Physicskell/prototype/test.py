import physics

board = physics.Board(width = 40, height = 40)

circ_1 = physics.Circle(x = 0, y = 35,
                        radius = 5, color = 'blue',
                        vel_x = 3, vel_y = -15)

circ_2 = physics.Circle(x = 0, y = 14,
                        radius = 2, color = 'green',
                        vel_x = 0, vel_y = 0)

circ_3 = physics.Circle(x = 0, y = 19,
                        radius = 2, color = 'yellow',
                        vel_x = 0, vel_y = 0)

@physics.wrapper
def frame():
    circ_1.force(0, 3)
    circ_2.force(0.5 * (17 - circ_2.x), 0)
    circ_3.force(0.6 * (14 - circ_3.x), 0)
    board.upload(circ_1, circ_2, circ_3)

if __name__ == '__main__':
    frame()
