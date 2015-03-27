from Tkinter import *
from time import sleep

CANVAS_WIDTH = 600
CANVAS_HEIGHT = 390

class Ball:
    def __init__(self):
        self.x = 0
        self.y = 0
        self.dx = 2
        self.dy = 2
        self.radius = 3
        self.color = "#ff0000"
    def update(self):
        if not (0 <= self.x <= CANVAS_WIDTH): self.dx *= -1
        if not (0 <= self.y <= CANVAS_HEIGHT): self.dy *= -1
        self.x += self.dx
        self.y += self.dy

if __name__ == '__main__':
    balls = []
    window = Tk()

    canvas = Canvas(window, bg="white", width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
    canvas.pack()

    add_ball_button = Button(window, text="Add", command=lambda:balls.append(Ball(canvas)))
    add_ball_button.pack()
    
    while True:
        sleep(.1)
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
