from Tkinter import *
from time import sleep, time
from random import randint, choice
import math

NUM_BALLS = 500

CANVAS_WIDTH = 1000
CANVAS_HEIGHT = 600

TARGET_FPS = 60.0
G_CONSTANT = -600 / TARGET_FPS

FRAME_DROP_LIMIT = 10

def set_text(entry_box, s):
    entry_box.delete(0, END)
    entry_box.insert(0, s)

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
        self.color = get_random_color() if 'color' not in kwargs.keys() else kwargs['color']
    def update(self):
        # balls don't collide with each other, and the math on edge collision is fudged
        # todo: improve edge collision detection by taking into account radius of the ball
        if not (0 <= self.x <= CANVAS_WIDTH): self.dx *= -1

        self.dy -= G_CONSTANT * TIME_STEP
        if (0 >= self.y) and (self.dy < 0):
            self.dy *= -1
        if (CANVAS_HEIGHT <= self.y) and (self.dy > 0): self.dy *= -1

        self.x += self.dx
        self.y += self.dy
    def __str__(self):
        return "<Ball at %i, %i>" % (self.x, self.y)

class GravityBall(Ball):
    def __init__(self, **kwargs):
        Ball.__init__(self, **kwargs)
        self.max_speed = math.sqrt(self.dy**2 + 2 * abs(self.y * G_CONSTANT * TARGET_FPS))
    def update(self):
        if not (0 <= self.x <= CANVAS_WIDTH): self.dx *= -1
        self.x += (self.dx / TARGET_FPS)

        self.dy += G_CONSTANT
        self.y += (self.dy / TARGET_FPS)
        if self.y < 0:
            self.y = 0
            self.dy = self.max_speed
        
class VisualBall(GravityBall):
    def __init__(self, canvas, **kwargs):
        GravityBall.__init__(self, **kwargs)
        self.ownHandleOnCanvas = canvas.create_oval(self.bbox(), fill=self.color)
    def bbox(self):
        return tuple([int(n) for n in (self.x, (CANVAS_HEIGHT - self.y), self.x+2*self.radius, (CANVAS_HEIGHT - self.y+2*self.radius))])
    def update(self):
        GravityBall.update(self)
        # tk-note: canvas.coords with two arguments is a setter operation
        canvas.coords(self.ownHandleOnCanvas, self.bbox())  # non-intuitive, see documentation

class TimeStepCanvas(Canvas):
    def __init__(self, parent, timestep, **kwargs):
        Canvas.__init__(self, parent, **kwargs)
        self.timestep = timestep
        self.nextScheduledStart = time() + self.timestep
        self.consecutiveDroppedFrames = 0
    def update(self):
        if self.nextScheduledStart > time() or self.consecutiveDroppedFrames >= FRAME_DROP_LIMIT:
            Canvas.update(self)
            self.consecutiveDroppedFrames = 0
            wasDrawn = True
        else:
            self.consecutiveDroppedFrames += 1
            #print "dropped a frame", self.consecutiveDroppedFrames
            wasDrawn = False

        waitTime = self.nextScheduledStart - time()
        if waitTime > 0: sleep(waitTime)

        self.nextScheduledStart += self.timestep
        return wasDrawn
        
class FPS_Canvas(TimeStepCanvas):
    def __init__(self, parent, target_fps, **kwargs):
        self.frame = Frame(parent)
        TimeStepCanvas.__init__(self, self.frame, 1 / target_fps, **kwargs)
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
            set_text(self.fps_readout, '%.1f' % (float(sum(self.lastFiveMeasurements)) / len(self.lastFiveMeasurements)))
            self.framesCounted = 0
            self.framesInLastSecond = []

def get_random_color():
    color = "#"
    for i in range(6):
        color += choice("0123456789abcdef")
    return color

DY_DEBUG_BOX = None
DEBUG_BOX_2 = None
DEBUG_BOX_3 = None

if __name__ == '__main__':
    # GUI stuff
    window = Tk()
    window.title("Bouncing Balls")
    canvas = FPS_Canvas(window, TARGET_FPS, bg="white", width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()
    frame = Frame(window)
    frame.grid_propagate(False)
    
    #balls = [VisualBall(canvas, x=250, y=600, radius=15, color='red', dx=0, dy=0)]
    
    balls = []
    for i in range(NUM_BALLS): balls.append(VisualBall(canvas))

    # animation loop
    while True:
        for ball in balls:
            ball.update()
        canvas.update()

    window.mainloop()  # not strictly necessary due to infinite loop
