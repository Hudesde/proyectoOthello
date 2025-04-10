import pygame
import sys

# Chessboard setup (example text-based representation)

file = open("boardText.txt",'r')
#leer dato...
m=eval(file.read())
#eval(m)
print(m)
for i in m:
    print(i)
file.close()
chessboard = m

# Chessboard and piece settings
square_size = 60
board_size = 8
white_square_color = (255, 215, 255)  # white squares
black_square_color = (255, 115, 255)  # white squares
#black_square_color = (125, 135, 150)  # black squares
highlight_color = (100, 255, 0)  # color for highlighting squares

# Piece images (or you can use text to represent pieces)
# For simplicity, we'll just represent pieces as text, but you can load images if desired.
piece_font = None

# Function to draw the chessboard
def draw_chessboard(screen):
    for row in range(board_size):
        for col in range(board_size):
            # Color the square
            color = white_square_color if (row + col) % 2 == 0 else black_square_color
            pygame.draw.rect(screen, color, pygame.Rect(col * square_size, row * square_size, square_size, square_size))
            
            # Draw the piece if there is one
            piece = chessboard[row][col]
            if piece != ".":
                piece_text = piece.upper() if piece.islower() else piece.lower()
                text_surface = piece_font.render(piece_text, True, (0, 0, 0) if piece.islower() else (255, 255, 255))
                text_rect = text_surface.get_rect(center=(col * square_size + square_size // 2, row * square_size + square_size // 2))
                screen.blit(text_surface, text_rect)

# Function to initialize pygame
def main():
    pygame.init()
    
    # Set up the display
    screen_width = board_size * square_size
    screen_height = board_size * square_size
    screen = pygame.display.set_mode((screen_width, screen_height))
    pygame.display.set_caption('2D Chessboard')

    global piece_font
    piece_font = pygame.font.Font(None, 40)

    clock = pygame.time.Clock()

    # Main loop
    while True:
        screen.fill((0, 0, 0))  # Fill the screen with black color
        draw_chessboard(screen)

        # Handle events
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()

        pygame.display.flip()
        clock.tick(30)

if __name__ == "__main__":
    main()

    #Dijkstra decía que separar preocupaciones (concerns) es algo
    #que ayuda mucho a programar bien.

