/***********************************************************************/
/*                                                                     */
/*                           Active dvi                                */
/*                                                                     */
/*            Jun Furuse, Pierre Weis, Didier Rémy                     */
/*            projet Cristal, INRIA Rocquencourt                       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <caml/fail.h>
#include "libgraph.h"
#include "image.h"

value gr_get_color(void)
{
  return Val_int(grcolor);
}

value gr_bsize_x(void)
{
  gr_check_open();
  return Val_int(grbstore.w);
}

value gr_draw_area(value im, value xywh, value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int wy = Wcvt(y) + 1 - Height_im(im);
  int by = Bcvt(y) + 1 - Height_im(im);
  int width = Int_val(Field (xywh, 2));
  int height = Int_val(Field (xywh, 3));
  int dx = Int_val(Field (xywh, 0));
  int dy = Int_val(Field (xywh, 1));

  gr_check_open();
  if (width > Width_im(im) - dx) width = Width_im(im) - dx;
  if (height > Height_im(im) - dy) height = Width_im(im) - dy;
  if (Mask_im(im) != None) {
    if(grremember_mode) {
      XSetClipOrigin(grdisplay, grbstore.gc, x, by);
      XSetClipMask(grdisplay, grbstore.gc, Mask_im(im));
    }
    if(grdisplay_mode) {
      XSetClipOrigin(grdisplay, grwindow.gc, x, wy);
      XSetClipMask(grdisplay, grwindow.gc, Mask_im(im));
    }
  }
  if(grremember_mode)
    XCopyArea(grdisplay, Data_im(im), grbstore.win, grbstore.gc,
              dx, dy,
              width, height,
              x, by);
  if(grdisplay_mode)
    XCopyArea(grdisplay, Data_im(im), grwindow.win, grwindow.gc,
          dx, dy,
          width, height,
          x, wy);
  if (Mask_im(im) != None) {
    if(grremember_mode)
      XSetClipMask(grdisplay, grbstore.gc, None);
    if(grdisplay_mode)
      XSetClipMask(grdisplay, grwindow.gc, None);
  }
  if(grdisplay_mode)
    XFlush(grdisplay);
  return Val_unit;
}

value gr_anti_synchronize(void)
{
  gr_check_open();
  XCopyArea(grdisplay, grwindow.win, grbstore.win, grwindow.gc,
            0, grbstore.h - grwindow.h,
            grwindow.w, grwindow.h,
            0, 0);
  XFlush(grdisplay);
  return Val_unit ;
}

value gr_window_color(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XImage * im;
  int rgb;

  gr_check_open();
  im = XGetImage(grdisplay, grwindow.win, x, Bcvt(y), 1, 1, (-1), ZPixmap);
  rgb = gr_rgb_pixel(XGetPixel(im, 0, 0));
  XDestroyImage(im);
  return Val_int(rgb);
}

value gr_bsize_y(void)
{
  gr_check_open();
  return Val_int(grbstore.h);
}

value gr_screen_x(void)
{
  XWindowAttributes att;
  gr_check_open();
  XGetWindowAttributes (grdisplay, DefaultRootWindow(grdisplay), &att); 
  return Val_int(att.width);
}

value gr_screen_y(void)
{
  XWindowAttributes att;
  gr_check_open();
  XGetWindowAttributes (grdisplay, DefaultRootWindow(grdisplay), &att); 
  return Val_int(att.height);
}

void gr_origin(int* x, int* y)
{
  Window win, root, parent, r;
  Window* children;
  int dx, dy, h, w, b, d; 
  gr_check_open();
  win = grwindow.win;
  root = DefaultRootWindow(grdisplay);
  *x = 0; *y = 0;
  while (win != root) {
    XGetGeometry (grdisplay, win, &r, &dx, &dy, &w, &h, &b, &d);
    *x = dx;
    *y = dy;
    XQueryTree (grdisplay, win, &r, &parent, &children, &b);
    win = parent;
  }
  return;
}

value gr_origin_x(void)
{
  int x, y;
  gr_origin (&x, &y); 
  return Val_int(x);
}
value gr_origin_y(void)
{
  int x, y;
  gr_origin (&x, &y); 
  return Val_int(y);
}

value gr_window(void)
{ unsigned int w; value res;
  gr_check_open();
  w = grwindow.win;
  return copy_int32 (w); 
}

value gr_bstore(void)
{ unsigned int w; value res;
  gr_check_open();
  w = grbstore.win;
  return copy_int32 (w); 
}

value gr_flush(void)
{
  gr_check_open();
  XFlush(grdisplay);
  return Val_unit ;
}

value gr_sync(void)
{
  gr_check_open();
  XSync(grdisplay, 0);
  return Val_unit ;
}

/* The following is not the best, since it unsets the selection 
   It would be better to own the selection. However, the event loop should
   then be changed */
value gr_cut (value string) {
  /* The following, suggested by Fabrice does not work */
  /* 
  Atom cut_buffers[] = 
     { XA_CUT_BUFFER0,
       XA_CUT_BUFFER1, 
       XA_CUT_BUFFER2, 
       XA_CUT_BUFFER3, 
       XA_CUT_BUFFER4, 
       XA_CUT_BUFFER5, 
       XA_CUT_BUFFER6, 
       XA_CUT_BUFFER7
     };
  XRotateWindowProperties (grdisplay, grwindow.win,
                           cut_buffers, 8, 1);
  */
  /* XRotateBuffers(grdisplay, 1); */
  /*
  XChangeProperty (grdisplay, grwindow.win, 
                   XA_CUT_BUFFER0,	      
                   XA_STRING,                  
                   8,       			
                   PropModeReplace, 		
                   String_val (string),
                   String_length (string)
                   );
  */
  XStoreBytes (grdisplay, String_val (string), string_length (string)); 
  XSetSelectionOwner (grdisplay,
                      XA_PRIMARY,
                      None,
                      CurrentTime);
  XSync (grdisplay, False);
  return Val_unit; 
}

value gr_set_named_atom_property (value name, value string) {
  Atom a = XInternAtom (grdisplay, String_val(name), 0);
  XChangeProperty (grdisplay, grwindow.win, 
                   a,       			/* property */ 
                   XA_STRING,                   /* xa_string */ 
                   8,       			/* format */
                   PropModeReplace, 		/* mode */
                   String_val(string), 		/* data */
                   string_length (string) 	/* nelements */
                   );
  XSync(grdisplay, False);
  return Val_unit;
}

value gr_set_cursor(value glyphid) {
  Cursor c;
  int gid;
  gid = Int_val(glyphid);
  gr_check_open();
  if (gid < 0 || gid >= XC_num_glyphs) {
    invalid_argument("set_cursor");
  }
  c = XCreateFontCursor(grdisplay, gid);
  XDefineCursor(grdisplay, grwindow.win, c);
  XSync(grdisplay, False);
  return Val_unit;
}

value gr_unset_cursor(value unit) {
  XUndefineCursor(grdisplay, grwindow.win);
  XSync(grdisplay, False);
  return Val_unit;
}

/* may not be correct ? may use the output of xwininfo ? */
void get_position_against_root( Window w, int *pos )
{
  Window root, parent;
  Window *children;
  int nchildren;
  XWindowAttributes attr;

  gr_check_open();
  XGetWindowAttributes(grdisplay, w, &attr);
  pos[0] += attr.x;
  pos[1] += attr.y;
  XQueryTree(grdisplay, w, &root, &parent, &children, &nchildren);
  if(children != NULL){
    XFree(children);
  }
  if( root == parent ){ 
    return; 
  } else {
    get_position_against_root( parent, pos );
  }
}

value gr_get_geometry(value unit){
  CAMLparam1(unit);
  CAMLlocal1(res);
  XWindowAttributes attr;
  int pos[2] = {0,0};

  gr_check_open();
  XGetWindowAttributes(grdisplay, grwindow.win, &attr);
  get_position_against_root( grwindow.win, pos );

  res = alloc_tuple(4);
  Field(res,0) = Val_int(attr.width);
  Field(res,1) = Val_int(attr.height);
  Field(res,2) = Val_int(pos[0]);
  Field(res,3) = Val_int(pos[1]);
  CAMLreturn(res);
}

/* get modifiers... */
value gr_get_modifiers(void)
{
  int mouse_x, mouse_y, button, key, keypressed;
  Window rootwin, childwin;
  int root_x, root_y, win_x, win_y;
  unsigned int modifiers;
  unsigned int i;

  gr_check_open();
  if (XQueryPointer(grdisplay, grwindow.win,
                    &rootwin, &childwin,
                    &root_x, &root_y, &win_x, &win_y,
                    &modifiers)) {
    button = 0;
    if (modifiers & Button1Mask) button = button | 0x1;
    if (modifiers & Button2Mask) button = button | 0x2;
    if (modifiers & Button3Mask) button = button | 0x4;
    if (modifiers & Button4Mask) button = button | 0x8;
    if (modifiers & Button5Mask) button = button | 0x10;
    
    if (modifiers & ShiftMask) button = button | 0x100;
    if (modifiers & ControlMask) button = button | 0x200;
    if (modifiers & Mod1Mask) button = button | 0x400;
    if (modifiers & Mod2Mask) button = button | 0x800;
    if (modifiers & Mod3Mask) button = button | 0x1000;
  } else {
    button = -1;
  }
  return Val_int(button);
}

/* Sub windows */
value gr_open_sub_window(value vx, value vy, value width, value height)
{
  Window win;

  int h = Int_val(height);
  int w = Int_val(width);
  int x = Int_val(vx);
  int y = Int_val(vy);

  gr_check_open();
  win = XCreateSimpleWindow(grdisplay, grwindow.win,
                            x, Wcvt(y + h), 
                            w, h,
                            0, grblack, grbackground);
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);

  return (id_of_window(win));
}

/* In graphics */
value gr_close_subwindow2(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XDestroyWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_map_window(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_unmap_window(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XUnmapWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_move_window (value wid, value grx, value gry, value height)
{
  Window win;

  int x = Int_val(grx);
  int y = Int_val(gry);
  int h = Int_val(height);

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMoveWindow(grdisplay, win, x, Wcvt(y + h));
  XFlush(grdisplay);
  return Val_unit;
}

value gr_resize_window (value wid, value w, value h)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XResizeWindow(grdisplay, win, Int_val(w), Int_val(h));
  XFlush(grdisplay);
  return Val_unit;
}


/* Window manager manipulation functions to handle "fullscreen" mode */
/* The code from here to gr_reposition is extracted and reorganized
   from the WM detection logic and fullscreen routines of mplayer    */

/* TODO : add motif detection to configure */

#ifdef HAVE_MOTIF

#include <X11/Xm/MwmUtil.h>

#else

/* bit definitions for MwmHints.flags */
#define MWM_HINTS_FUNCTIONS	(1L << 0)
#define MWM_HINTS_DECORATIONS	(1L << 1)
#define MWM_HINTS_INPUT_MODE	(1L << 2)
#define MWM_HINTS_STATUS	(1L << 3)

/* bit definitions for MwmHints.functions */
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)

/* bit definitions for MwmHints.decorations */
#define MWM_DECOR_ALL		(1L << 0)
#define MWM_DECOR_BORDER	(1L << 1)
#define MWM_DECOR_RESIZEH	(1L << 2)
#define MWM_DECOR_TITLE		(1L << 3)
#define MWM_DECOR_MENU		(1L << 4)
#define MWM_DECOR_MINIMIZE	(1L << 5)
#define MWM_DECOR_MAXIMIZE	(1L << 6)

typedef struct
{
    unsigned long	flags;
    unsigned long	functions;
    unsigned long	decorations;
    long 	        inputMode;
    unsigned long	status;
} PropMotifWmHints;

#define PROP_MOTIF_WM_HINTS_ELEMENTS	5

#endif
static Atom XA_NET_SUPPORTED;
static Atom XA_NET_WM_STATE;
static Atom XA_NET_WM_STATE_FULLSCREEN;
static Atom XA_NET_WM_STATE_ABOVE;
static Atom XA_NET_WM_STATE_STAYS_ON_TOP;
static Atom XA_NET_WM_STATE_BELOW;
static Atom XA_NET_WM_PID;
static Atom XA_WIN_PROTOCOLS;
static Atom XA_WIN_LAYER;
static Atom XA_WIN_HINTS;
static Atom XA_BLACKBOX_PID;

static void init_atoms(Display * mDisplay)
{
 XA_NET_SUPPORTED  = XInternAtom(mDisplay, "_NET_SUPPORTED", 0 ) ;
 XA_NET_WM_STATE  = XInternAtom(mDisplay, "_NET_WM_STATE", 0 ) ;
 XA_NET_WM_STATE_FULLSCREEN  = XInternAtom(mDisplay, "_NET_WM_STATE_FULLSCREEN", 0 ) ;
 XA_NET_WM_STATE_ABOVE  = XInternAtom(mDisplay, "_NET_WM_STATE_ABOVE", 0 ) ;
 XA_NET_WM_STATE_STAYS_ON_TOP  = XInternAtom(mDisplay, "_NET_WM_STATE_STAYS_ON_TOP", 0 ) ;
 XA_NET_WM_STATE_BELOW  = XInternAtom(mDisplay, "_NET_WM_STATE_BELOW", 0 ) ;
 XA_NET_WM_PID  = XInternAtom(mDisplay, "_NET_WM_PID", 0 ) ;
 XA_WIN_PROTOCOLS  = XInternAtom(mDisplay, "_WIN_PROTOCOLS", 0 ) ;
 XA_WIN_LAYER  = XInternAtom(mDisplay, "_WIN_LAYER", 0 ) ;
 XA_WIN_HINTS  = XInternAtom(mDisplay, "_WIN_HINTS", 0 ) ;
 XA_BLACKBOX_PID  = XInternAtom(mDisplay, "_BLACKBOX_PID", 0 ) ;
}

typedef struct
{
  long flags;
  long functions;
  long decorations;
  long input_mode;
  long state;
} MotifWmHints;


void x11_decoration( Display *dpy,Window w,int decorate, int fsmode )
{
  MotifWmHints MotifWmHints;
  Atom MotifHints;

  if(fsmode&1){
    XSetWindowAttributes attr;
    attr.override_redirect = (!decorate) ? True : False;
    XChangeWindowAttributes(dpy, w, CWOverrideRedirect, &attr);
  }

  if(fsmode&8){
    XSetTransientForHint (dpy, w, RootWindow(dpy,DefaultScreen(dpy)));
  }

 MotifHints=XInternAtom( dpy,"_MOTIF_WM_HINTS",0 );
 if ( MotifHints != None )
  {
   memset( &MotifWmHints,0,sizeof( MotifWmHints ) );
   MotifWmHints.flags=MWM_HINTS_FUNCTIONS | MWM_HINTS_DECORATIONS;
   if ( decorate )
    {
      MotifWmHints.functions=MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE;
      decorate=MWM_DECOR_ALL;
    }
   
   MotifWmHints.decorations=decorate|((fsmode&2)?0:MWM_DECOR_MENU);

   XChangeProperty( dpy,w,MotifHints,MotifHints,32,
                    PropModeReplace,(unsigned char *)&MotifWmHints,(fsmode&4)?4:5 );
  }
}

void x11_sizehint(Display *mDisplay, Window window, int x, int y, int width, int height, int max )
{
XSizeHints hint;

 hint.flags=PPosition | PSize | PWinGravity;

 hint.x=x; hint.y=y; hint.width=width; hint.height=height;
 if ( max )
  {
   hint.max_width=width; hint.max_height=height;
   hint.flags|=PMaxSize;
  } else { hint.max_width=0; hint.max_height=0; }
 hint.win_gravity=StaticGravity;
 XSetWMNormalHints( mDisplay,window,&hint );
}

static int x11_get_property(Display *dpy, Window w,Atom type, Atom **args, unsigned long *nitems)
{
  int             format;
  unsigned long   bytesafter;

  return (Success == XGetWindowProperty( dpy,w,type,0,16384,
                                         False,AnyPropertyType,&type,&format,nitems,&bytesafter,
                                         (unsigned char **) args ) && *nitems > 0 );
}

#define wm_LAYER 1
#define wm_FULLSCREEN 2
#define wm_STAYS_ON_TOP 4
#define wm_ABOVE 8
#define wm_BELOW 16
#define wm_NETWM (wm_FULLSCREEN | wm_STAYS_ON_TOP | wm_ABOVE | wm_BELOW)

static int net_wm_support_state_test(Atom atom)
{
#define NET_WM_STATE_TEST(x) { if (atom == XA_NET_WM_STATE_##x) { return wm_##x; } }
 
 NET_WM_STATE_TEST(FULLSCREEN);
 NET_WM_STATE_TEST(ABOVE);
 NET_WM_STATE_TEST(STAYS_ON_TOP);
 NET_WM_STATE_TEST(BELOW);
 return 0;
}

static int wm_detect(Display *dpy, Window w)
{
 int             i;
 int             wm = 0;
 int             metacity_hack = 0;
 unsigned long   nitems;
 Atom          * args = NULL;
 
// -- supports layers
  if (x11_get_property(dpy, w,XA_WIN_PROTOCOLS, &args, &nitems))
  {
   for (i = 0; i < nitems; i++)
   {
     if ( args[i] == XA_WIN_LAYER) {
       wm |= wm_LAYER;
       metacity_hack |= 1;
     } else
       // metacity is the only manager mplayer's authors know which reports support only for _WIN_LAYER
       // hint in _WIN_PROTOCOLS (what's more support for it, they say, is broken)
       metacity_hack |= 2;
   }
   XFree( args );
   if (wm && (metacity_hack == 1))
   {
     // metacity reports that it supports layers, but it is not really truth :-)
     wm ^= wm_LAYER;
   }
  }

// --- netwm 
  if (x11_get_property(dpy,w,XA_NET_SUPPORTED, &args, &nitems))
  {
   for (i = 0; i < nitems; i++)
     wm |= net_wm_support_state_test (args[i]);
   XFree( args );
  }

 return wm;
}    

void x11_ontop( Display * dpy,Window w,int fs_type, int fs)
{
   XClientMessageEvent  xev;
   char *state;

   memset( &xev,0,sizeof( xev ) );
   xev.type= 33 ;
   xev.message_type=XA_NET_WM_STATE;
   xev.display=dpy;
   xev.window=w;
   xev.format=32;
   xev.data.l[0]=fs;

   if ( fs_type & wm_STAYS_ON_TOP )
     xev.data.l[1]=XA_NET_WM_STATE_STAYS_ON_TOP;
   else
   if ( fs_type & wm_ABOVE )
     xev.data.l[1]=XA_NET_WM_STATE_ABOVE;
   else
   if ( fs_type & wm_FULLSCREEN )
     xev.data.l[1]=XA_NET_WM_STATE_FULLSCREEN;
   else
   if ( fs_type & wm_BELOW )
     // This is not fallback. We can safely assume that situation where
     // only NETWM_STATE_BELOW is supported and others not, doesn't exist.
     xev.data.l[1]=XA_NET_WM_STATE_BELOW;

   xev.data.l[1]=XA_NET_WM_STATE_STAYS_ON_TOP;
   
   XSendEvent(dpy,RootWindow(dpy,DefaultScreen(dpy)),0 ,(1L<<20) ,(XEvent*)&xev );
   state = XGetAtomName (dpy, xev.data.l[1]);
   XFree (state);
}

value gr_reposition (value x, value y, value w, value h)
{
  Window win;
  XWindowAttributes att;

  int width, height;
  int decorate;
  int fs_style; /* choose the appropriate wm style for full screen, as done in mplayer */

  gr_check_open();

  XGetWindowAttributes (grdisplay, grwindow.win, &att);

  /* create the X atoms: should be done only once */
  init_atoms(grdisplay);

  /* try to figure out what kind of fs capabilities the wm offers */
  fs_style=wm_detect(grdisplay, DefaultRootWindow(grdisplay));

  width = Int_val(w); height = Int_val(h);
  XGetWindowAttributes (grdisplay, DefaultRootWindow(grdisplay), &att);
  if (width < 0) { 
    width = att.width; height = att.height;
  }; 

  /* Should we decorate the window? We choose according to fullscreen or not */

  if (width==att.width && height==att.height)
    { decorate = 0; /*fullscreen, no decoration */}
  else { decorate = 1; /*not fullscreen, restore decoration */}

  x11_decoration(grdisplay,grwindow.win,decorate,fs_style);

  /* Giving size hints is _essential_ for the "fullscreen" effect, at least in KDE */

  x11_sizehint(grdisplay,grwindow.win,Int_val(x), Int_val(y), width, height,0 );

  XMoveResizeWindow(grdisplay, grwindow.win, 
                    Int_val(x), Int_val(y), width, height);

  /* Now make sure our window is in front of all the others */
  x11_ontop(grdisplay, grwindow.win, fs_style, !decorate); /* !decorate is true iff we are in fs mode */

    grwindow.w = width;
    grwindow.h = height;
    if (grwindow.w > grbstore.w || grwindow.h > grbstore.h) {

      /* Allocate a new backing store large enough to accomodate
         both the old backing store and the current window. */
      struct canvas newbstore;
      newbstore.w = max(grwindow.w, grbstore.w);
      newbstore.h = max(grwindow.h, grbstore.h);
      newbstore.win =
        XCreatePixmap(grdisplay, grwindow.win, newbstore.w, newbstore.h,
                      XDefaultDepth(grdisplay, grscreen));
      newbstore.gc = XCreateGC(grdisplay, newbstore.win, 0, NULL);
      XSetBackground(grdisplay, newbstore.gc, grwhite);
      XSetForeground(grdisplay, newbstore.gc, grwhite);
      XFillRectangle(grdisplay, newbstore.win, newbstore.gc,
                     0, 0, newbstore.w, newbstore.h);
      XSetForeground(grdisplay, newbstore.gc, grcolor);
      if (grfont != NULL)
        XSetFont(grdisplay, newbstore.gc, grfont->fid);

      /* Copy the old backing store into the new one */
      XCopyArea(grdisplay, grbstore.win, newbstore.win, newbstore.gc,
                0, 0, grbstore.w, grbstore.h, 0, newbstore.h - grbstore.h);

      /* Free the old backing store */
      XFreeGC(grdisplay, grbstore.gc);
      XFreePixmap(grdisplay, grbstore.win);

      /* Use the new backing store */
      grbstore = newbstore;
    }

  XFlush(grdisplay);
  return Val_unit;
}
