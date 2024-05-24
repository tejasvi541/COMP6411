import socketserver
import os
import re

class CustomerDB:
    def __init__(self, filename):
        self.filename = filename
        self.db = {}
        self.load_db()

    def load_db(self):
        with open(self.filename, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    fields = line.split('|')
                    fields = [field.strip() for field in fields]
                    if len(fields) != 4:
                        continue
                    name, age, address, phone = fields
                    if not re.match('^[a-zA-Z]+$', name): # To match only alphabets
                        print(f"Invalid name in record: {line}")
                        continue
                    if age and not (age.isdigit() and 1 <= int(age) <= 120):
                        print(f"Invalid age in record: {line}")
                        continue
                    if address and not re.match('^[a-zA-Z0-9 .-]+$', address): # To match only alphabets, numbers, space, dot and hyphen
                        print(f"Invalid address in record: {line}")
                        continue
                    if phone and not re.match('^(\d{3} \d{3}-\d{4}|\d{3}-\d{4})$', phone): # To match phone number in format XXX XXX-XXXX or XXX-XXXX
                        print(f"Invalid phone number in record: {line}")
                        continue
                    self.db[name.lower()] = (name, age, address, phone)

    def find_customer(self, name):
        return self.db.get(name.lower(), None)

    def add_customer(self, name, age, address, phone):
        if name.lower() in self.db:
            return False
        self.db[name.lower()] = (name, age, address, phone)
        self.write_db()
        return True

    def delete_customer(self, name):
        if name.lower() in self.db:
            del self.db[name.lower()]
            self.write_db()
            return True
        else:
            return False

    def update_customer(self, name, age=None, address=None, phone=None):
        if name.lower() not in self.db:
            return False
        old_name, old_age, old_address, old_phone = self.db[name.lower()]
        self.db[name.lower()] = (name, age or old_age, address or old_address, phone or old_phone)
        self.write_db()
        return True

    def print_report(self):
        return sorted(self.db.values(), key=lambda x: x[0])

    def write_db(self):
        with open(self.filename, 'w') as f:
            for name, (name, age, address, phone) in self.db.items():
                f.write(f'{name}|{age}|{address}|{phone}\n')

class ServerHandler(socketserver.BaseRequestHandler):
    def handle(self):
        while True:
            self.data = self.request.recv(1024).strip().decode() # Receive data from client 1024 bytes at a time, also decodes it
            print("{} wrote:".format(self.client_address[0]))
            print(self.data)
            if not self.data:
                break
            response = self.process_request(self.data)
            self.request.sendall(response.encode())

    def process_request(self, request):
        parts = request.split('|')
        command = parts[0]
        args = parts[1:]
        if command == 'find':
            customer = self.server.db.find_customer(*args)
            if customer:
                return '|'.join(customer)
            else:
                return 'Customer not found'
        elif command == 'add':
            if self.server.db.add_customer(*args):
                return 'Customer added'
            else:
                return 'Customer already exists'
        elif command == 'delete':
            if self.server.db.delete_customer(*args):
                return 'Customer deleted'
            else:
                return 'Customer not found'
        elif command == 'update':
            if self.server.db.update_customer(*args):
                return 'Customer updated'
            else:
                return 'Customer not found'
        elif command == 'report':
            report = self.server.db.print_report()
            return '\n'.join('|'.join(record) for record in report)
        else:
            return 'Invalid command'

if __name__ == "__main__":
    HOST, PORT = "localhost", 9999
    with socketserver.TCPServer((HOST, PORT), ServerHandler) as server:
        server.db = CustomerDB('data.txt')
        print("Server started")
        server.serve_forever()