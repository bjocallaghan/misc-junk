from Tkinter import *  # the simple windowing and graphics library that Python for Windows includes
from time import sleep  # importing a library function; vanilla Python has a LOT of built-in libs
from random import randint, choice # choice is kinda nice. randomly gets one element from a sequence

CANVAS_WIDTH = 600  # python doesn't really have constants, just variables you promise not to change
CANVAS_HEIGHT = 390

# Logan can modify the behavior of the object without fully understanding OOP principles
class Ball:
    def __init__(self):  # the constructor. name is always "__init__"
        # location
        self.x = randint(0, CANVAS_WIDTH)  # randint takes 2 arguments, both inclusive
        self.y = randint(0, CANVAS_HEIGHT)
        # delta/speed
        self.dx = randint(1,8) * choice([1,-1])  # [1,-1] is a two-element list, both integers
        self.dy = randint(1,8) * choice([1,-1])
        # non-changing ball attributes
        self.radius = randint(2,15)
        self.color = get_random_color()
    def update(self):  # instance methods always have implicit "self" as first argument
        # balls don't collide with each other, and the math on edge collision is fudged
        # todo: improve edge collision detection by taking into account radius of the ball
        if not (0 <= self.x <= CANVAS_WIDTH): self.dx *= -1
        if not (0 <= self.y <= CANVAS_HEIGHT): self.dy *= -1
        self.x += self.dx
        self.y += self.dy

def get_random_color():  # no notion of difference between subs and functions; this is a function
    color = "#"  # string we'll append to
    for i in range(6):  # Python's "for loop" idiom; it's just a twisted foreach. has pros/cons
        color += choice("0123456789abcdef")  # choice function works on strings too
    return color

if __name__ == '__main__':  # the "main" idiom; not necessary, but suppose you wanted to import Ball
    balls = []  # an empty list (at the start)

    # GUI stuff
    window = Tk()
    window.title("Bouncing Balls")
    canvas = Canvas(window, bg="white", width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()
    # in line below, command can be a lambda or an actual function. functions are first-class objs
    add_ball_button = Button(window, text="Add", command=lambda:balls.append(Ball()))
    add_ball_button.pack()
    
    # animation loop
    while True:
        sleep(.02)  # set the speed here; speed is inversely proportional to sleep time

        canvas.delete("ball")  # get rid of all the old oval objects stored in/on the canvas
        for ball in balls:  # Python's foreach idiom
            ball.update()  # balls will self-update their internal state (but not their visual)
            canvas.create_oval(ball.x,  # oval specification: upper left, lower right
                               ball.y,
                               ball.x+2*ball.radius,
                               ball.y+2*ball.radius,
                               fill = ball.color,
                               tags = "ball")  # draw a new oval, also tag it with the "ball" label
        canvas.update()

    # you would usually need this, but here it isn't necessary because of the infinite loop
    window.mainloop()
