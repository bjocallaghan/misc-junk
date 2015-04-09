#include <gtk/gtk.h>
#include <cairo.h>
#include <math.h>
#include <time.h>


static char buffer[256];

void hello (void)
{
  g_print ("Hello World\n");
}

void destroy (void)
{
  gtk_main_quit ();
}

int xc = 60;
int yc = 60;
int dirx = 2;
int diry = 2;

/* Expose Event
   Draw everthing here */
static gboolean on_expose_event (GtkWidget *widget,
                                 GdkEventExpose *event,
                                 gpointer data)
{
  cairo_t *cr;
  int width, height;
  
  cr = gdk_cairo_create (widget->window);
  gtk_window_get_size(GTK_WINDOW(widget), &width, &height);

  if ((xc > width) || (xc < 0)) {
    dirx = dirx * -1;
  }

  if ((yc > height) || (yc < 0)) {
    diry = diry * -1;
  }

  xc = xc + dirx;
  yc = yc + diry;

  cairo_set_line_width(cr, 9);

  cairo_set_source_rgb(cr, 0.69, 0.19, 0);
  cairo_arc(cr, xc, yc, 
            20, 0, 2 * M_PI);
  cairo_stroke_preserve(cr);

  cairo_set_source_rgb(cr, 0.3, 0.4, 0.6); 
  cairo_fill(cr);

  cairo_destroy(cr);

  return FALSE;
}

/* Timer event 
   Forces a window expose event.
   Consider this the main loop (framerate controlled) */
static gboolean time_handler(GtkWidget *widget)
{
  if (widget->window == NULL) return FALSE;

  time_t curtime;
  struct tm *loctime;

  curtime = time(NULL);
  loctime = localtime(&curtime);
  strftime(buffer, 256, "%T", loctime);

  //printf("%s\n", buffer); 

  gtk_widget_queue_draw(widget);
  return TRUE;
}

int main (int argc, char *argv[])
{
  GtkWidget *window;
  GdkColor b = { 0, 0, 0, 0};

  gtk_init (&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect (GTK_OBJECT (window), "destroy",
                      GTK_SIGNAL_FUNC (destroy), NULL);
  gtk_container_border_width (GTK_CONTAINER (window), 10);

  gtk_window_fullscreen(GTK_OBJECT(window));
  gtk_widget_modify_bg(GTK_OBJECT(window),GTK_STATE_NORMAL,&b); 

  gtk_widget_show (window);

  g_signal_connect(G_OBJECT(window), "expose-event",
                   G_CALLBACK(on_expose_event), NULL);
  g_signal_connect(G_OBJECT(window), "destroy",
                   G_CALLBACK(gtk_main_quit), NULL);

  g_timeout_add(33, (GSourceFunc) time_handler, (gpointer) window); 

  gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
  gtk_window_set_default_size(GTK_WINDOW(window), 200, 150); 

  gtk_widget_set_app_paintable(window, TRUE);
  gtk_widget_show_all(window);

  time_handler(window);

  gtk_main ();

  return 0;
}
