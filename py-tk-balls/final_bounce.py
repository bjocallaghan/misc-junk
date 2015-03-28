from Tkinter import Tk, Canvas, Entry, Frame, Label, RIGHT, END
from time import sleep, time
from random import randint, random
import math

NUM_BALLS = 60

CANVAS_WIDTH = 1000
CANVAS_HEIGHT = 600

TARGET_FPS = 60.0
G_CONSTANT = -500.0


class Ball:
    def __init__(self, **kwargs):
        # location
        self.x = kwargs.get('x', randint(0, CANVAS_WIDTH))
        self.y = kwargs.get('y', randint(0, CANVAS_HEIGHT))
        # delta/speed
        self.dx = kwargs.get('dx', randint(-500, 500))
        self.dy = kwargs.get('dy', randint(-200, 200))
        # non-changing ball attributes
        self.radius = kwargs.get('radius', randint(2, 15))
        # energy calculations
        energy = abs(self.y * G_CONSTANT) + .5 * self.dy**2
        self.max_speed = math.sqrt(2 * energy)
        self.temp_max_speed = random() * self.max_speed  # start: rand fraction
        # initial coloration
        self.hue = get_random_hue_values()
        self.color = rgb_to_color(self.hue)

    def update(self):
        self.x += (self.dx / TARGET_FPS)
        if not (self.radius <= self.x <= CANVAS_WIDTH-self.radius):
            self.dx *= -1

        self.dy += G_CONSTANT / TARGET_FPS
        self.y += (self.dy / TARGET_FPS)
        if (self.y-self.radius) < 0:
            self.y = self.radius
            self.temp_max_speed *= .8
            if self.temp_max_speed < .2 * self.max_speed:
                self.temp_max_speed = self.max_speed
            self.dy = self.temp_max_speed

        hue_fraction = math.sqrt(1 - (abs(self.dy) / self.max_speed))
        self.color = rgb_to_color([hue_fraction*x for x in self.hue])

    def __str__(self):
        return "<Ball at %i, %i>" % (self.x, self.y)


class VisualBall(Ball):
    def __init__(self, canvas, **kwargs):
        Ball.__init__(self, **kwargs)
        self.canvas_handle = canvas.create_oval(tuple(self.bbox()),
                                                fill=self.color)

    def bbox(self):
        return [int(n) for n in [self.x - self.radius,
                                 (CANVAS_HEIGHT - self.y + self.radius),
                                 self.x + self.radius,
                                 (CANVAS_HEIGHT - self.y - self.radius)]]

    def update(self):
        Ball.update(self)
        # tk-note: setter operations on canvas objects; non-intuitive
        canvas.coords(self.canvas_handle, tuple(self.bbox()))
        canvas.itemconfig(self.canvas_handle, fill=self.color)


class TimeStepCanvas(Canvas):
    def __init__(self, parent, time_step, drop_limit, **kwargs):
        Canvas.__init__(self, parent, **kwargs)
        self.time_step = time_step
        self.next_start = time() + self.time_step
        self.consec_dropped = 0
        self.drop_limit = drop_limit

    def update(self):
        if self.next_start > time() or self.consec_dropped >= self.drop_limit:
            Canvas.update(self)
            self.consec_dropped = 0
            wasDrawn = True
        else:
            self.consec_dropped += 1
            wasDrawn = False
        waitTime = self.next_start - time()
        if waitTime > 0:
            sleep(waitTime)
        self.next_start += self.time_step
        return wasDrawn
        

class FPS_Canvas(TimeStepCanvas):
    def __init__(self, parent, target_fps, drop_limit=10, **kwargs):
        self.frame = Frame(parent)
        TimeStepCanvas.__init__(self, self.frame, 1.0 / target_fps,
                                drop_limit, **kwargs)
        self.fps_readout = Entry(self.frame)
        self.target_fps = target_fps
        self.framesCounted = 0
        self.framesInLastSecond = []
        self.lastFiveMeasurements = []

    def pack(self):
        self.frame.pack()
        TimeStepCanvas.pack(self)
        self.fps_readout.pack(side=RIGHT)
        Label(self.frame, text="FPS:").pack(side=RIGHT)

    def update_fps_readout(self, value):
        self.fps_readout.delete(0, END)
        self.fps_readout.insert(0, '%.1f' % value)

    def update(self):
        if TimeStepCanvas.update(self):
            self.framesInLastSecond.append(True)
        else:
            self.framesInLastSecond.append(False)
        self.framesCounted += 1
        if self.framesCounted == self.target_fps:
            fps_measurement = len([x for x in self.framesInLastSecond if x])
            self.lastFiveMeasurements.append(fps_measurement)
            if len(self.lastFiveMeasurements) == 6:
                self.lastFiveMeasurements = self.lastFiveMeasurements[1:]
            avg = (float(sum(self.lastFiveMeasurements)) /
                   len(self.lastFiveMeasurements))
            self.update_fps_readout(avg)
            self.framesCounted = 0
            self.framesInLastSecond = []


def get_random_hue_values():
    rand_rgb_values = (randint(1, 255), randint(1, 255), randint(1, 255))
    return [int(255*float(x)/max(rand_rgb_values)) for x in rand_rgb_values]


def rgb_to_color(rgb_values):
    return '#%02x%02x%02x' % tuple(rgb_values)


if __name__ == '__main__':
    # GUI stuff
    window = Tk()
    window.title("%d Bouncing Balls" % NUM_BALLS)
    canvas = FPS_Canvas(window, TARGET_FPS, bg="white",
                        width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()

    # balls
    balls = []
    for i in range(NUM_BALLS):
        balls.append(VisualBall(canvas))

    # animation loop
    while True:
        for ball in balls:
            ball.update()
        canvas.update()

    window.mainloop()  # not strictly necessary due to infinite loop
