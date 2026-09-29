import math
import time
import multiprocessing as mp
import subprocess
import cv2
import numpy
import mss
import csv
import pylink
import sys
import os
from pynput import mouse
from pynput import keyboard


def time_since_start():
    return (time.time() - time_since_start.start_time)*1000


time_since_start.start_time = time.time()


def on_release(key, queue_csv):
    time_keyboard = time_since_start()
    queue_csv.put([time_keyboard, "Keyboard",
                   f"Key: {key} (Released)"])
    if str(key) == "'s'" or str(key) == "'S'":
        return False


def on_click(x, y, button, pressed, queue_csv):
    time_mouse = time_since_start()
    queue_csv.put([time_mouse, "Mouse",
                   f"X:{x}; Y:{y}; MouseEvent:{button}{('.Pressed' if pressed else '.Released')}"])


def on_scroll(x, y, dx, dy, queue_csv):
    time_mouse = time_since_start()
    queue_csv.put([time_mouse, "Mouse",
                   f"X:{x}; Y:{y}; MouseEvent:WM_MOUSEWHEEL; ScrollDelta:{'-120' if dy < 0 else '120'}"])


def grab(queue: mp.Queue, fps, queue_csv: mp.Queue, start_time, queue_termination: mp.Queue,
         monitor_width, monitor_height):
    monitor = {"top": 0, "left": 0, "width": monitor_width, "height": monitor_height}
    number_frames = 0
    with mss.mss() as sct:
        start_time_process = (time.time() - start_time)*1000
        while "Screen capturing":
            time_elapsed = (time.time() - start_time)*1000 - start_time_process

            ticks = math.floor(time_elapsed / ((1/fps)*1000)) - number_frames
            if ticks >= 1:
                time_before_grab = (time.time() - start_time)*1000
                img = numpy.array(sct.grab(monitor))
                time_grabbed = ((time.time() - start_time)*1000 + time_before_grab)/2
                if number_frames == 0:
                    queue_csv.put([time_grabbed, "Video recorder",
                                   "Started recording (first frame)"])
                for i in range(ticks):
                    queue_csv.put([time_grabbed, "Video recorder",
                                   f"Frame {(number_frames + i)}{' (skipped)' if i > 0 else ''}"])
                queue.put((img, ticks))
                number_frames += ticks
            if not queue_termination.empty():
                queue.put(None)
                break


def record(queue: mp.Queue, fps, queue_csv: mp.Queue, monitor_width, monitor_height, file_name, video_file_name):

    fourcc = cv2.VideoWriter_fourcc(*'mp4v')
    vid = cv2.VideoWriter(f'{video_file_name}.mp4', fourcc, fps, (monitor_width, monitor_height))
    file = open(f'{file_name}.csv', "w", newline="")
    writer = csv.writer(file)
    writer.writerow(["Timestamp", "Source", "Data"])
    while "Recording":
        if not queue.empty():
            img_queue_element = queue.get()
            if img_queue_element is None:
                break
            img = img_queue_element[0]
            img = cv2.cvtColor(img, cv2.COLOR_BGRA2BGR)
            for i in range(img_queue_element[1]):
                vid.write(img)
        if not queue_csv.empty():
            writer.writerow(queue_csv.get())
    file.close()


if __name__ == '__main__':

    PID = input("PID: ")
    PID_before = PID
    PID = PID[-8:]
    monitor_width = 1920
    monitor_height = 1080
    video_folder = os.path.join(f"Videos")
    if not os.path.exists(video_folder):
        os.makedirs(video_folder)
    folder = os.path.join(f"Data")
    file_name = f'{PID}.edf'
    if not os.path.exists(folder):
        os.makedirs(folder)
    local_file_name = os.path.join(folder, f'{PID_before}.edf')

    website = r"https://bldrrreview.eu.pythonanywhere.com/?user=" + PID_before

    fps = 20.0
    queue: mp.Queue = mp.Queue()
    queue_csv: mp.Queue = mp.Queue()
    queue_termination: mp.Queue = mp.Queue()

    mouse_listener = mouse.Listener(
        on_click=lambda x, y, button, pressed: on_click(x, y, button, pressed, queue_csv),
        on_scroll=lambda x, y, dx, dy: on_scroll(x, y, dx, dy, queue_csv))
    mouse_listener.start()
    grabbing = mp.Process(target=grab, args=(queue, fps, queue_csv, time_since_start.start_time,
                                             queue_termination, monitor_width, monitor_height))
    recording = mp.Process(target=record, args=(queue, fps, queue_csv, monitor_width, monitor_height,
                                                os.path.join(folder,
                                                             f'{PID_before}'),
                                                os.path.join(video_folder,
                                                             f'{PID_before}')))
    grabbing.start()
    recording.start()
    start = time_since_start()
    browser = subprocess.Popen([r"C:\Program Files\Google\Chrome\Application\chrome.exe",
                                website],
                               start_new_session=True)
    with keyboard.Listener(on_release=lambda key: on_release(key, queue_csv)) as keyboard_listener:
        try:
            eye_tracker = pylink.EyeLink("100.1.1.1")
        except RuntimeError as error:
            print('ERROR:', error)
            sys.exit()
        pylink.openGraphics((0, 0), 32)

        eye_tracker.openDataFile(file_name)
        eye_tracker.setOfflineMode()
        eye_tracker.sendCommand("sample_rate 1000")
        eye_tracker.sendCommand(f'screen_pixel_coords 0 0 {monitor_width - 1} {monitor_height - 1}')
        eye_tracker.sendMessage(f'DISPLAY_COORDS 0 0 {monitor_width - 1} {monitor_height - 1}')
        eye_tracker.doTrackerSetup()
        pylink.closeGraphics()
        eye_tracker.startRecording(1, 1, 0, 0)
        eye_tracker.sendMessage(f'SYNCTIME')
        queue_csv.put([time_since_start(), "Synchronizer", "SYNCTIME"])
        keyboard_listener.join()
        eye_tracker.stopRecording()
        eye_tracker.setOfflineMode()
        pylink.msecDelay(250)
        eye_tracker.closeDataFile()
        eye_tracker.receiveDataFile(file_name, local_file_name)
        eye_tracker.stopRecording()
        eye_tracker.close()

    queue_termination.put(None)
    recording.join()
    mouse_listener.stop()
    pylink.msecDelay(1000)
