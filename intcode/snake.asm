# Startup

0000: rel_base +=  1200

# Draw screen

0002: SCREEN_Y = 0
0006: SCREEN_X = 0
0010: self[rel_base + 1] = SCREEN_X
0014: self[rel_base + 2] = SCREEN_Y
0018: self[rel_base] = 25
0022: GOTO get_tile
0025: OUTPUT SCREEN_X
0027: OUTPUT SCREEN_y
0029: OUTPUT self[rel_base + 1]
0031: SCREEN_X = SCREEN_X + 1
0035: FLAG = SCREEN_X < WIDTH
0039: IF FLAG GOTO 10
0042: SCREEN_Y = SCREEN_Y + 1
0046: FLAG = SCREEN_Y < HEIGHT
0050: IF FLAG GOTO 6

# Initial score

0053: OUTPUT -1
0055: OUTPUT 0
0057: OUTPUT SCORE

# Get input

0059: INPUT --> INPUT_DIR
0061: FLAG = INPUT_DIR == 0
0065: IF FLAG GOTO get_velocity
0068: FLAG = -1 * SNAKE_DIR
0072: FLAG = FLAG + INPUT_DIR
0076: FLAG = FLAG * FLAG
0080: FLAG = FLAG == 4
0084: IF FLAG GOTO get_velocity
0087: SNAKE_DIR = INPUT_DIR

# Get velocity

0091: self[0096] = (x_velocities - 1) + SNAKE_DIR
0095: DX = self[self[0096]]
0099: self[104] = (y_velocities - 1) + SNAKE_DIR
0103: DY = self[self[104]]

# Move snake

0107: HEAD_X = HEAD_X + DX
0111: HEAD_Y = HEAD_y + DY
0115: HEAD_IDX = HEAD_IDX + 1
0119: self[134] = snake_data + HEAD_IDX
0123: self[132] = HEAD_Y * WIDTH
0127: self[132] = self[132] + HEAD_X
0131: self[self[134]] = self[132]

# Get data

0135: TAIL_X = self[snake_data]
0139: TAIL_Y = 0
0143: FLAG = TAIL_X < WIDTH
0147: IF FLAG GOTO 161
0150: TAIL_X = TAIL_X - WIDTH
0154: TAIL_Y = TAIL_Y + 1
0158: GOTO 143
0161: self[rel_base + 1] = HEAD_X
0165: self[rel_base + 2] = HEAD_Y
0169: self[rel_base] = 176
0173: GOTO get_tile
0176: TILE = self[rel_base + 1]
0180: FLAG = TILE == 2
0184: IF FLAG GOTO output_snake

# Shift snake

0187: POS = 0
0191: FLAG = POS > HEAD_IDX
0195: IF FLAG GOTO 217
0198: self[209] = POS + snake_data
0202: self[207] = self[209] + 1
0206: self[self[209]] = self[self[207]]
0210: POS = POS + 1
0214: GOTO 191
0217: HEAD_IDX = HEAD_IDX - 1
0221: self[rel_base + 1] = TAIL_X
0225: self[rel_base + 2] = TAIL_Y
0229: self[rel_base + 3] = 0
0233: self[rel_base] = 240
0237: GOTO set_tile

# Check collision

0240: FLAG = TILE - 2
0244: FLAG = FLAG * FLAG
0248: FLAG = FLAG == 1
0252: IF !FLAG GOTO 256
0255: EXIT

# Output snake

0256: self[rel_base + 1] = HEAD_X
0260: self[rel_base + 2] = HEAD_Y
0264: self[rel_base + 3] = 3
0268: self[rel_base] = 275
0272: GOTO set_tile
0275: OUTPUT HEAD_X
0277: OUTPUT HEAD_Y
0279: OUTPUT 3
0281: FLAG = TILE == 2
0285: IF FLAG GOTO update_score
0288: OUTPUT TAIL_X
0290: OUTPUT TAIL_Y
0292: OUTPUT 0
0294: GOTO get_input

# Update score

0297: SCORE = SCORE + 100
0301: OUTPUT -1
0303: OUTPUT 0
0305: OUTPUT SCORE
0307: FLAG = HEAD_IDX == MAX_LENGTH
0311: IF !FLAG GOTO 315
0314: EXIT

# Find blob

0315: BLOB_IDX = BLOB_IDX + 1
0319: FLAG = BLOB_IDX == MAX_LENGTH
0323: IF !FLAG GOTO 330
0326: BLOB_IDX = BLOB_IDX - MAX_LENGTH

0330: self[335] = blob_data + BLOB_IDX
0334: BLOB_X = self[self[335]]
0338: BLOB_Y = 0
0342: FLAG = BLOB_X < WIDTH
0346: IF FLAG GOTO 360
0349: BLOB_X = BLOB_X - WIDTH
0353: BLOB_Y = BLOB_Y + 1
0357: GOTO 342
0360: self[rel_base + 1] = BLOB_X
0364: self[rel_base + 2] = BLOB_Y
0368: self[rel_base] = 375
0372: GOTO get_tile
0375: FLAG = self[rel_base + 1] == 0
0379: IF !FLAG GOTO 315

# Write blob

0382: self[rel_base + 1] = BLOB_X
0386: self[rel_base + 2] = BLOB_Y
0390: self[rel_base + 3] = 2
0394: self[rel_base] = 401
0398: GOTO set_tile
0401: OUTPUT BLOB_X
0403: OUTPUT BLOB_Y
0405: OUTPUT 2
0407: GOTO get_input

# Get tile

0410: self[423] = self[rel_base + 2] * WIDTH
0414: self[423] = self[423] + self[rel_base + 1]
0418: self[423] = self[423] + tile_data
0422: self[rel_base + 1] = self[self[423]]
0426: GOTO self[rel_base]

# Set tile

0429: self[444] = self[rel_base + 2] * WIDTH
0433: self[444] = self[444] + self[rel_base + 1]
0437: self[444] = self[444] + tile_data
0441: self[self[444]] = self[rel_base + 3]
0445: GOTO self[rel_base]

# Data

0448: FLAG
0449: SCREEN_X
0450: SCREEN_Y
0451: SCORE
0452: INPUT_DIR
0453: SNAKE_DIR
0454: DX
0455: DY
0456: POS
0457: HEAD_X
0458: HEAD_Y
0459: TAIL_X
0460: TAIL_Y
0461: HEAD_IDX
0462: TILE
0463: BLOB_IDX
0464: BLOB_X
0465: BLOB_Y

# Tile data

0466 ---> 0745

# Snake data

0746 ---> 0962

# Velocities x data

0963 ---> 0966

# Velocities y data

0967 ---> 0970

# Blob data

0971 ---> 1186
