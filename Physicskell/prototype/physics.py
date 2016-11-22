import sys
import traceback
from time import sleep

try:
    import colorama
    colorama.init()
except ImportError:
    pass

# -------------------------------------------------
# globals

write = sys.stdout.write

table_color = {
    'black' : 30,
    'red' : 31,
    'green' : 32,
    'yellow' : 33,
    'blue' : 34,
    'magenta' : 35,
    'cyan' : 36,
    'white' : 37
}

color_default = 'white'

dt = 0.02

scale_dt = 5

# -------------------------------------------------
# utils

def wrapper(func):
    def run_func():
        try:
            while 1:
                func()
        except KeyboardInterrupt:
            write('\033[%dm\033[2J' % table_color['white'])
            write('Terminated by user.\n')
        except:
            write('\033[%dm\033[2J' % table_color['white'])
            traceback.print_exc()

    return run_func

# -------------------------------------------------
# objects

class Object(object):
    def __init__(self,
                 x = 0, y = 0,
                 mass = 1,
                 vel_x = 0, vel_y = 0,
                 color = 'green', unit = '#',
                 is_collidable = False):
        self.x, self.y = x, y
        self.mass = float(mass)
        self.vel_x, self.vel_y = vel_x, vel_y
        self.color = color
        self.unit = unit
        self.is_collidable = is_collidable

        # 'type' : a tag for using in collision, etc.
        self.type = 'object'

        # initial (total) force (= acceleration * mass) = 0
        self.accel_x, self.accel_y = 0, 0

    def force(self, x, y):
        self.accel_x += x / self.mass
        self.accel_y += y / self.mass

    def update(self):
        self.vel_x += self.accel_x * dt * scale_dt
        self.vel_y += self.accel_y * dt * scale_dt

        self.x += self.vel_x * dt * scale_dt
        self.y += self.vel_y * dt * scale_dt

        # re-initiallize the total force
        self.accel_x, self.accel_y = 0, 0

    # override this!
    def collide_point(self, other):
        return False

    # override this!
    def collide_rectangle(self, other):
        return False

    # override this!
    def collide_circle(self, other):
        return False

    def collide(self, other):
        if not (self.is_collidable and other.is_collidable):
            return False

        t = other.type

        if t == 'point':
            return self.collide_point(other)
        elif t == 'rectangle':
            return self.collide_rectangle(other)
        elif t == 'circle':
            return self.collide_circle(other)
        else:
            return False

class Point(Object):
    def __init__(self, *args, **kwargs):
        super(Point, self).__init__(*args, **kwargs)
        self.type = 'point'
        self.pixels = [(0, 0)]

    def collide_point(self, other):
        return (self.x == other.x) and (self.y == other.y)

    def collide_rectangle(self, other):
        return False

    def collide_circle(self, other):
        return False

class Rectangle(Object):
    def __init__(self, width, height, *args, **kwargs):
        super(Rectangle, self).__init__(*args, **kwargs)
        self.type = 'rectangle'
        self.width, self.height = width, height

        self.pixels = [
            (i, j)
            for j in xrange(self.height)
            for i in xrange(self.width)
        ]

class Circle(Object):
    def __init__(self, radius, *args, **kwargs):
        super(Circle, self).__init__(*args, **kwargs)
        self.type = 'circle'
        self.radius = radius

        self.pixels = [
            (i, j)
            for j in xrange(self.radius * 2)
            for i in xrange(self.radius * 2)
            if (i - self.radius) ** 2 + (j - self.radius) ** 2 \
            < self.radius ** 2
        ]

# -------------------------------------------------
# board

class Board(object):
    def __init__(self, width, height):
        self.width, self.height = width, height
        self.buffer = []

        # clear the screen at start
        write('\033[2J')
        self.clear()
        self.flush()

    def clear(self):
        row = '. ' * self.width

        self.buffer.append(
            '\033[1;1H%s' % '\n'.join(row for i in xrange(self.height))
        )

    def draw(self, obj):
        w, h = self.width, self.height
        x, y = int(obj.x), int(obj.y)
        ch = obj.unit
        append = self.buffer.append

        append('\033[%dm' % table_color[obj.color])

        for (i, j) in obj.pixels:
            if (1 <= x + i and x + i <= w and 1 <= y + j and y + j <= h):
                append('\033[%d;%dH%s' % (y + j, 2 * (x + i) - 1, ch))

        append('\033[%dm' % table_color[color_default])

    def flush(self):
        write(''.join(self.buffer))
        self.buffer = []

    def upload(self, *list_objs):
        self.clear()

        for obj in list_objs:
            obj.update()
            self.draw(obj)

        self.flush()
        sleep(dt)

