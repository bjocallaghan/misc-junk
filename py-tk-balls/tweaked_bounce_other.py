from Tkinter import *
from time import sleep
from random import randint, choice

NUM_BALLS = 3000

CANVAS_WIDTH = 600
CANVAS_HEIGHT = 390

class Ball:
    def __init__(self):
        self.x = randint(0, CANVAS_WIDTH)
        self.y = randint(0, CANVAS_HEIGHT)
        self.dx = randint(1,8) * choice([1,-1])
        self.dy = randint(1,8) * choice([1,-1])
        self.radius = randint(2,15)
        self.color = get_random_color()
    def update(self):
        if not (0 <= self.x <= CANVAS_WIDTH): self.dx *= -1
        if not (0 <= self.y <= CANVAS_HEIGHT): self.dy *= -1
        self.x += self.dx
        self.y += self.dy

def get_random_color():
    color = "#"
    for i in range(6):
        color += choice("0123456789abcdef")
    return color

if __name__ == '__main__':
    balls = []
    for i in range(NUM_BALLS): balls.append(Ball())

    window = Tk()
    window.title("Bouncing Balls")

    canvas = Canvas(window, bg="white", width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()

    add_ball_button = Button(window, text="Add", command=lambda:balls.append(Ball()))
    add_ball_button.pack()
    
    while True:
        sleep(.02)
        canvas.delete("ball")
        for ball in balls:
            ball.update()
            canvas.create_oval(ball.x,
                               ball.y,
                               ball.x+2*ball.radius,
                               ball.y+2*ball.radius,
                               fill = ball.color,
                               tags = "ball")
        canvas.update()
    window.mainloop()
