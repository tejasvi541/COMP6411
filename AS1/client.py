import socket
import os
import re

def send_request(sock, request)->str:
    sock.sendall(request.encode())
    response = sock.recv(1024).decode()
    return response

def validate_name(name):
    if not re.match(r"^[a-zA-Z]+$", name):
        print("The name must be present and consist only of alphabetic characters. Please try again.")
        return False
    return True

def validate_age(age):
    if age == "" or (age.isdigit() and 1 <= int(age) <= 120):
        return True
    print("Age must be an integer between 1 and 120. Please try again.")
    return False

def validate_address(address):
    if address == "" or re.match(r"^[a-zA-Z0-9\s.-]*$", address.strip()):
        return True
    print("If address is present, it can only contain alphanumeric characters, spaces, periods, or dashes. Please try again.")
    return False

def validate_phone_number(phone_number):
    if phone_number == "" or re.match(r'^(\d{3} \d{3}-\d{4}|\d{3}-\d{4})$', phone_number.strip()):
        return True
    print("Invalid phone number format. Please try again.")
    return False

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
                flag = True
                while flag:
                    name = input('Enter customer name: ').strip()
                    age = input('Enter customer age: ').strip()
                    address = input('Enter customer address: ').strip()
                    phone = input('Enter customer phone: ').strip()
                    flag = not (validate_name(name) and validate_age(age) and  validate_address(address) and  validate_phone_number(phone))
                response = send_request(sock, f'add|{name.lower()}|{age}|{address}|{phone}')
                print('Server response:', response)
            elif choice == '3':
                name = input('Enter customer name: ').strip()
                response = send_request(sock, f'delete|{name}')
                print('Server response:', response)
            elif choice == '4':
                flag = True
                while flag:
                    name = input('Enter customer name: ').strip()
                    age = input('Enter new age: ').strip()
                    flag = not (validate_name(name) and validate_age(age))
                response = send_request(sock, f'update|{name}|{age}')
                print('Server response:', response)
            elif choice == '5':
                flag = True
                while flag:
                    name = input('Enter customer name: ').strip()
                    age = input('Enter new age: ').strip()
                    flag = not (validate_name(name) and validate_address(age))
                response = send_request(sock, f'update|{name}||{address}')
                print('Server response:', response)
            elif choice == '6':
                flag = True
                while flag:
                    name = input('Enter customer name: ').strip()
                    age = input('Enter new age: ').strip()
                    flag = not (validate_name(name) and validate_phone_number(age))
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
            os.system('clear')

if __name__ == "__main__":
    main()