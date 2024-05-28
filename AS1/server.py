import socketserver
import re

STATUS_CODES = {
    "OK": 200,
    "NOT_FOUND": 404,
    "BAD_REQUEST": 400,
    "CONFLICT": 409
}
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
    if phone_number == "" or re.match(r'^(\d{3}\s\d{3}-\d{4}|\d{3}-\d{4}){0,1}$', phone_number.strip()):
        return True
    print("Invalid phone number format. Please try again.")
    return False
class CustomerDB:
    def __init__(self, filename):
        self.filename = filename
        self.db = {}
        self.load_db()

    def load_db(self)->None:
        with open(self.filename, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    fields = line.split('|')
                    fields = [field.strip() for field in fields]
                    if len(fields) != 4:
                        print(f"Record Skipped Invalid record: {line}")
                        continue
                    name, age, address, phone = fields
                    if(not(validate_name(name) and validate_age(age) and  validate_address(address) and  validate_phone_number(phone))):
                        print(f"Record Skipped Invalid record: {line}")
                        continue
                    self.db[name.lower()] = (name.lower(), age, address, phone)

    def find_customer(self, name)->tuple:
        return self.db.get(name.lower(), None)

    def add_customer(self, name, age, address, phone)->int:
        if name.lower() in self.db:
            return STATUS_CODES['CONFLICT']
        if not re.match('^[a-zA-Z]+$', name): # To match only alphabets
            print(f"Invalid name in record")
            return STATUS_CODES['BAD_REQUEST']
        if age and not (age.isdigit() and 1 <= int(age) <= 120):
            print(f"Invalid age in record")
            return STATUS_CODES['BAD_REQUEST']
        if address and not re.match('^[a-zA-Z0-9 .-]+$', address): # To match only alphabets, numbers, space, dot and hyphen
            print(f"Invalid address in record")
            return STATUS_CODES['BAD_REQUEST']
        if phone and not re.match('^(\d{3}\s\d{3}-\d{4}|\d{3}-\d{4}){0,1}$', phone): # To match phone number in format XXX XXX-XXXX or XXX-XXXX
            print(f"Invalid phone number in record")
            return STATUS_CODES['BAD_REQUEST']
        self.db[name.lower()] = (name.lower(), age, address, phone)
        return STATUS_CODES['OK']
        

    def delete_customer(self, name)->int:
        if name.lower() in self.db:
            del self.db[name.lower()]
            return STATUS_CODES['OK']
        else:
            return STATUS_CODES['NOT_FOUND']

    def update_customer(self, name, age=None, address=None, phone=None)->int:
        if name.lower() not in self.db:
            return STATUS_CODES['NOT_FOUND']
        if not re.match('^[a-zA-Z]+$', name): # To match only alphabets
            print(f"Invalid name in record")
            return STATUS_CODES['BAD_REQUEST']
        if age and not (age.isdigit() and 1 <= int(age) <= 120):
            print(f"Invalid age in record")
            return STATUS_CODES['BAD_REQUEST']
        if address and not re.match('^[a-zA-Z0-9 .-]+$', address): # To match only alphabets, numbers, space, dot and hyphen
            print(f"Invalid address in record")
            return STATUS_CODES['BAD_REQUEST']
        if phone and not re.match('^(\d{3} \d{3}-\d{4}|\d{3}-\d{4})$', phone): # To match phone number in format XXX XXX-XXXX or XXX-XXXX
            print(f"Invalid phone number in record")
            return STATUS_CODES['BAD_REQUEST']
        old_name, old_age, old_address, old_phone = self.db[name.lower()]
        self.db[name.lower()] = (name.lower(), age or old_age, address or old_address, phone or old_phone)
        return STATUS_CODES['OK']

    def print_report(self)->list:
        return sorted(self.db.values(), key=lambda x: x[0])

class ServerHandler(socketserver.BaseRequestHandler):
    def handle(self)->None:
        while True:
            self.data = self.request.recv(1024).strip().decode() # Receive data from client 1024 bytes at a time, also decodes it
            print("{} wrote:".format(self.client_address[0]))
            print(self.data)
            if not self.data:
                break
            response = self.process_request(self.data)
            self.request.sendall(response.encode())
            

    def process_request(self, request)->str:
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
            if self.server.db.add_customer(*args)==STATUS_CODES['OK']:
                return 'Customer added'
            elif self.server.db.add_customer(*args)==STATUS_CODES['CONFLICT']:
                return 'Customer already exists'
            else:
                return 'Invalid data'
        elif command == 'delete':
            if self.server.db.delete_customer(*args) == STATUS_CODES['OK']:
                return 'Customer deleted'
            elif self.server.db.delete_customer(*args) == STATUS_CODES['NOT_FOUND']:
                return 'Customer not found'
            else:
                return 'Invalid data'
        elif command == 'update':
            if self.server.db.update_customer(*args)==STATUS_CODES['OK']:
                return 'Customer updated'
            elif self.server.db.update_customer(*args)==STATUS_CODES['NOT_FOUND']:
                return 'Customer not found'
            else:
                return 'Invalid data'
        elif command == 'report':
            report = self.server.db.print_report()
            return "\n*** DateBase Contents ***\n"+'\n'.join('|'.join(record) for record in report)
        else:
            return 'Invalid command'

if __name__ == "__main__":
    HOST, PORT = "localhost", 9999
    with socketserver.TCPServer((HOST, PORT), ServerHandler) as server:
        server.db = CustomerDB('data.txt')
        print("Server started")
        server.serve_forever()