import cv2
import math
import glob
import pandas as pd


def write_cross(series, img_top, img_bot):
    if series.get("img") == "top":
        cv2.line(img_top, (int(series.get("Corrected.Fixation.X")) - 20, int(series.get("Corrected.Fixation.Y.img")) - 20),
                 (int(series.get("Corrected.Fixation.X")) + 20, int(series.get("Corrected.Fixation.Y.img")) + 20), (255, 0, 0), 5)
        cv2.line(img_top, (int(series.get("Corrected.Fixation.X")) - 20, int(series.get("Corrected.Fixation.Y.img")) + 20),
                 (int(series.get("Corrected.Fixation.X")) + 20, int(series.get("Corrected.Fixation.Y.img")) - 20), (255, 0, 0), 5)
        cv2.putText(img_top, series.get("N_frame_video"),
                    (int(series.get("Corrected.Fixation.X")) - 60, int(series.get("Corrected.Fixation.Y.img")) - 30),
                    cv2.FONT_HERSHEY_SIMPLEX, 1, (255, 0, 0))
    else:
        cv2.line(img_bot, (int(series.get("Corrected.Fixation.X")) - 20, int(series.get("Corrected.Fixation.Y.img")) - 20),
                 (int(series.get("Corrected.Fixation.X")) + 20, int(series.get("Corrected.Fixation.Y.img")) + 20), (255, 0, 0), 5)
        cv2.line(img_bot, (int(series.get("Corrected.Fixation.X")) - 20, int(series.get("Corrected.Fixation.Y.img")) + 20),
                 (int(series.get("Corrected.Fixation.X")) + 20, int(series.get("Corrected.Fixation.Y.img")) - 20), (255, 0, 0), 5)
        cv2.putText(img_bot, series.get("N_frame_video"),
                    (int(series.get("Corrected.Fixation.X")) - 60, int(series.get("Corrected.Fixation.Y.img")) - 30),
                    cv2.FONT_HERSHEY_SIMPLEX, 1, (255, 0, 0))

    return img_top, img_bot


extension = 'csv'
files = glob.glob('./ET_check/*.{}'.format(extension))
start_coordinate = 274
shift_coordinate = 380
x_left = 316
x_right = 1602
y_shift = 295

list_roi = dict()
img_top = cv2.imread('img_top.png')
img_top_copy = img_top.copy()
img_bot = cv2.imread('img_bot.png')
img_bot_copy = img_bot.copy()
order = pd.read_csv("./order_files/order.csv")
alpha = 0.6

# Create a red overlay to check what the AOIs are
for i in range(len(order.Index)):
    if start_coordinate + i * shift_coordinate < 28800:
        cv2.rectangle(img_top, (x_left, math.floor(start_coordinate + i * shift_coordinate)), (x_right, math.floor(start_coordinate + i * shift_coordinate) + y_shift), (0, 0, 255), cv2.FILLED)
        if (math.floor(start_coordinate + i * shift_coordinate) + y_shift) >= 28800:
            cv2.rectangle(img_bot, (x_left, 0), (x_right, y_shift - (28800 - math.floor(start_coordinate + i * shift_coordinate))), (0, 0, 255), cv2.FILLED)    

    else:
        cv2.rectangle(img_bot, (x_left, math.floor(start_coordinate + i * shift_coordinate) - 28800), (x_right, math.floor(start_coordinate + i * shift_coordinate) + y_shift - 28800), (0, 0, 255), cv2.FILLED)


img_top = cv2.addWeighted(img_top, alpha, img_top_copy, 1 - alpha, gamma=0)
img_bot = cv2.addWeighted(img_bot, alpha, img_bot_copy, 1 - alpha, gamma=0)
cv2.imwrite(f'./images_output/top_overlay.png', img_top)
cv2.imwrite(f'./images_output/bot_overlay.png', img_bot)

for file in files:
    filename = file.split("\\")[-1]
    d_check = pd.read_csv(f"./ET_check/{filename}")
    img_top = cv2.imread('img_top.png')
    img_bot = cv2.imread('img_bot.png')

    # Copy each argument and put them in a dictionary (keys = index), using the "order.csv" template
    for i in range(len(order.Index)):
        if start_coordinate + i * shift_coordinate < 28800:
            list_roi[order.iloc[i]["Index"]] = (img_top[(math.floor(start_coordinate + i * shift_coordinate)):(math.floor(start_coordinate + i * shift_coordinate) + y_shift), x_left:x_right].copy())
            if (math.floor(start_coordinate + i * shift_coordinate) + y_shift) >= 28800:
                list_roi[order.iloc[i]["Index"]] = cv2.vconcat([list_roi[order.iloc[i]["Index"]], (img_bot[0:(y_shift - (28800 - math.floor(start_coordinate + i * shift_coordinate))), x_left:x_right].copy())])
        else:
            list_roi[order.iloc[i]["Index"]] = (img_bot[(math.floor(start_coordinate + i * shift_coordinate) - 28800):(math.floor(start_coordinate + i * shift_coordinate) + y_shift - 28800), x_left:x_right].copy())

    order_file = pd.read_csv(f"./order_files/{filename}")

    # Paste the right argument at the right place
    for j in range(len(order_file.Index)):
        if start_coordinate + j * shift_coordinate < 28800:
            if (math.floor(start_coordinate + j * shift_coordinate) + y_shift) >= 28800:
                img_top[(math.floor(start_coordinate + j * shift_coordinate)):28799, x_left:x_right] = list_roi[order_file.iloc[j]["Index"]][0:(28799-(math.floor(start_coordinate + j * shift_coordinate))), :]
                img_bot[0:(y_shift - (28800 - math.floor(start_coordinate + j * shift_coordinate))), x_left:x_right] = list_roi[order_file.iloc[j]["Index"]][(28800-(math.floor(start_coordinate + j * shift_coordinate))):, :]
            else:
                img_top[(math.floor(start_coordinate + j * shift_coordinate)):(math.floor(start_coordinate + j * shift_coordinate) + y_shift), x_left:x_right] = list_roi[order_file.iloc[j]["Index"]]
            
        else:
            img_bot[(math.floor(start_coordinate + j * shift_coordinate) - 28800):(math.floor(start_coordinate + j * shift_coordinate) + y_shift - 28800), x_left:x_right] = list_roi[order_file.iloc[j]["Index"]]

    # Write crosses
    for index, row in d_check.iterrows():
        img_top, img_bot = write_cross(row, img_top=img_top, img_bot=img_bot)

    cv2.imwrite(f'./images_output/{filename}_top.png', img_top)
    cv2.imwrite(f'./images_output/{filename}_bot.png', img_bot)
