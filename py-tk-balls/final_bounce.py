from Tkinter import *
from time import sleep, time
from random import randint, choice, random
import math

NUM_BALLS = 350

CANVAS_WIDTH = 1000
CANVAS_HEIGHT = 600

TARGET_FPS = 60.0
G_CONSTANT = -1000 / TARGET_FPS

class Ball:
    def __init__(self, **kwargs):
        # location
        self.x = randint(0, CANVAS_WIDTH) if 'x' not in kwargs.keys() else kwargs['x']
        self.y = randint(0, CANVAS_HEIGHT) if 'y' not in kwargs.keys() else kwargs['y']
        # delta/speed
        self.dx = randint(-500,500)  if 'dx' not in kwargs.keys() else kwargs['dx']
        self.dy = randint(-200,200) if 'dy' not in kwargs.keys() else kwargs['dy']
        # non-changing ball attributes
        self.radius = randint(2,15) if 'radius' not in kwargs.keys() else kwargs['radius']
        # energy calculations
        self.max_speed = math.sqrt(self.dy**2 + 2 * abs(self.y * G_CONSTANT * TARGET_FPS))
        self.temp_max_speed = random() * self.max_speed
        # initial coloration
        self.hue = get_random_hue_values()
        self.color = rgb_to_color_string(self.hue)
    def update(self):
        if not (0 <= self.x <= CANVAS_WIDTH): self.dx *= -1
        self.x += (self.dx / TARGET_FPS)

        self.dy += G_CONSTANT
        self.y += (self.dy / TARGET_FPS)
        if self.y < 0:
            self.y = 0
            self.temp_max_speed *= .8
            if self.temp_max_speed < .2 * self.max_speed: self.temp_max_speed = self.max_speed
            self.dy = self.temp_max_speed

        color_multiplier = math.sqrt(1 - (abs(self.dy) / self.max_speed))
        self.color = rgb_to_color_string([color_multiplier*x for x in self.hue])
    def __str__(self):
        return "<Ball at %i, %i>" % (self.x, self.y)
        
class VisualBall(Ball):
    def __init__(self, canvas, **kwargs):
        Ball.__init__(self, **kwargs)
        self.ownHandleOnCanvas = canvas.create_oval(self.bbox(), fill=self.color)
    def bbox(self):
        return tuple([int(n) for n in (self.x, (CANVAS_HEIGHT - self.y), self.x+2*self.radius, (CANVAS_HEIGHT - self.y+2*self.radius))])
    def update(self):
        Ball.update(self)
        # tk-note: setter operations on canvas objects
        canvas.coords(self.ownHandleOnCanvas, self.bbox())  # non-intuitive, see documentation
        canvas.itemconfig(self.ownHandleOnCanvas, fill=self.color)  # non-intuitive, see documentation

class TimeStepCanvas(Canvas):
    def __init__(self, parent, timestep, frame_drop_limit, **kwargs):
        Canvas.__init__(self, parent, **kwargs)
        self.timestep = timestep
        self.nextScheduledStart = time() + self.timestep
        self.consecutiveDroppedFrames = 0
        self.frame_drop_limit = frame_drop_limit
    def update(self):
        if self.nextScheduledStart > time() or self.consecutiveDroppedFrames >= self.frame_drop_limit:
            Canvas.update(self)
            self.consecutiveDroppedFrames = 0
            wasDrawn = True
        else:
            self.consecutiveDroppedFrames += 1
            wasDrawn = False
        waitTime = self.nextScheduledStart - time()
        if waitTime > 0: sleep(waitTime)
        self.nextScheduledStart += self.timestep
        return wasDrawn
        
class FPS_Canvas(TimeStepCanvas):
    def __init__(self, parent, target_fps, frame_drop_limit=10, **kwargs):
        self.frame = Frame(parent)
        TimeStepCanvas.__init__(self, self.frame, 1 / target_fps, frame_drop_limit, **kwargs)
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
            self.update_fps_readout(float(sum(self.lastFiveMeasurements)) / len(self.lastFiveMeasurements))
            self.framesCounted = 0
            self.framesInLastSecond = []

def get_random_hue_values():
    random_color_values = (randint(1,255), randint(1,255), randint(1,255))
    return [int(255*float(x)/max(random_color_values)) for x in random_color_values]

def rgb_to_color_string(rgb_values):
    return '#%02x%02x%02x' % tuple(rgb_values)

if __name__ == '__main__':
    # GUI stuff
    window = Tk()
    window.title("Bouncing Balls")
    canvas = FPS_Canvas(window, TARGET_FPS, bg="white", width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()
    
    balls = []
    for i in range(NUM_BALLS): balls.append(VisualBall(canvas))

    # animation loop
    while True:
        for ball in balls:
            ball.update()
        canvas.update()

    window.mainloop()  # not strictly necessary due to infinite loop
