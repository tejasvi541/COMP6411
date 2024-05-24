import socket
import sys

def send_request(sock, request):
    sock.sendall(request.encode())
    response = sock.recv(1024).decode()
    return response

def main():
    HOST, PORT = "localhost", 9999

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.connect((HOST, PORT))

        while True:
            print('Customer Management Menu')
            print('1. Find customer')
            print('2. Add customer')
            print('3. Delete customer')
            print('4. Update customer age')
            print('5. Update customer address')
            print('6. Update customer phone')
            print('7. Print report')
            print('8. Exit')
            print('Select: ', end='')
            choice = input().strip()

            if choice == '1':
                name = input('Enter customer name: ').strip()
                response = send_request(sock, f'find|{name}')
                print('Server response:', response)
            elif choice == '2':
                name = input('Enter customer name: ').strip()
                age = input('Enter customer age: ').strip()
                address = input('Enter customer address: ').strip()
                phone = input('Enter customer phone: ').strip()
                response = send_request(sock, f'add|{name}|{age}|{address}|{phone}')
                print('Server response:', response)
            elif choice == '3':
                name = input('Enter customer name: ').strip()
                response = send_request(sock, f'delete|{name}')
                print('Server response:', response)
            elif choice == '4':
                name = input('Enter customer name: ').strip()
                age = input('Enter new age: ').strip()
                response = send_request(sock, f'update|{name}|{age}')
                print('Server response:', response)
            elif choice == '5':
                name = input('Enter customer name: ').strip()
                address = input('Enter new address: ').strip()
                response = send_request(sock, f'update|{name}||{address}')
                print('Server response:', response)
            elif choice == '6':
                name = input('Enter customer name: ').strip()
                phone = input('Enter new phone: ').strip()
                response = send_request(sock, f'update|{name}|||{phone}')
                print('Server response:', response)
            elif choice == '7':
                response = send_request(sock, 'report')
                print('Server response:', response)
            elif choice == '8':
                print('Good bye')
                break
            else:
                print('Invalid choice')

            input('Press any key to continue...')

if __name__ == "__main__":
    main()