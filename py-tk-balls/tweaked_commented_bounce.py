from Tkinter import *
from time import sleep, time
from random import randint, choice

NUM_BALLS = 900

CANVAS_WIDTH = 600
CANVAS_HEIGHT = 390

TIME_STEP = .02
G_CONSTANT = -50
G_CONSTANT = 0

FRAME_DROP_LIMIT = 10

class Ball:
    def __init__(self):
        # location
        self.x = randint(0, CANVAS_WIDTH)
        self.y = randint(0, CANVAS_HEIGHT)
        # delta/speed
        self.dx = randint(1,8) * choice([1,-1])
        self.dy = randint(1,8) * choice([1,-1])
        # non-changing ball attributes
        self.radius = randint(2,15)
        self.color = get_random_color()
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

class VisualBall(Ball):
    def __init__(self, canvas):
        Ball.__init__(self)
        self.ownHandleOnCanvas = canvas.create_oval(self.bbox(), fill=self.color)
    def bbox(self):
        return (self.x, self.y, self.x+2*self.radius, self.y+2*self.radius)
    def update(self):
        Ball.update(self)
        # canvas.coords with two arguments is a setter operation
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
        else:
            self.consecutiveDroppedFrames += 1
            print "dropped a frame", self.consecutiveDroppedFrames

        waitTime = self.nextScheduledStart - time()
        if waitTime > 0: sleep(waitTime)

        self.nextScheduledStart += self.timestep
        
def get_random_color():
    color = "#"
    for i in range(6):
        color += choice("0123456789abcdef")
    return color

if __name__ == '__main__':
    # GUI stuff
    window = Tk()
    window.title("Bouncing Balls")
    canvas = TimeStepCanvas(window, TIME_STEP, bg="white", width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()
    
    balls = []
    for i in range(NUM_BALLS): balls.append(VisualBall(canvas))

    # animation loop
    while True:
        for ball in balls:
            ball.update()
        canvas.update()

    window.mainloop()  # not strictly necessary due to infinite loop
