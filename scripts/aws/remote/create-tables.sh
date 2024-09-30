aws dynamodb create-table \
    --table-name Person \
    --attribute-definitions AttributeName=PersonID,AttributeType=S \
    --key-schema AttributeName=PersonID,KeyType=HASH \
    --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5

aws dynamodb create-table \
    --table-name Student \
    --attribute-definitions AttributeName=StudentID,AttributeType=S \
    --key-schema AttributeName=StudentID,KeyType=HASH \
    --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5

aws dynamodb create-table \
    --table-name Teacher \
    --attribute-definitions AttributeName=TeacherID,AttributeType=S \
    --key-schema AttributeName=TeacherID,KeyType=HASH \
    --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5

# Add Initial Data to Person Table
aws dynamodb put-item \
    --table-name Person \
    --item '{
        "PersonID": {"S": "001"},
        "Name": {"S": "John Doe"},
        "Age": {"N": "40"},
        "Address": {"S": "123 Main St, New York"}
    }'

aws dynamodb put-item \
    --table-name Person \
    --item '{
        "PersonID": {"S": "002"},
        "Name": {"S": "Jane Smith"},
        "Age": {"N": "28"},
        "Address": {"S": "456 Oak St, Los Angeles"}
    }'
# Add Initial Data to Student Table
aws dynamodb put-item \
    --table-name Student \
    --item '{
        "StudentID": {"S": "S001"},
        "Name": {"S": "Emily Johnson"},
        "Age": {"N": "20"},
        "Major": {"S": "Computer Science"}
    }'

aws dynamodb put-item \
    --table-name Student \
    --item '{
        "StudentID": {"S": "S002"},
        "Name": {"S": "Michael Brown"},
        "Age": {"N": "22"},
        "Major": {"S": "Mathematics"}
    }'
# Add Initial Data to Teacher Table
aws dynamodb put-item \
    --table-name Teacher \
    --item '{
        "TeacherID": {"S": "T001"},
        "Name": {"S": "Dr. Alice White"},
        "Age": {"N": "45"},
        "Department": {"S": "Physics"}
    }'

aws dynamodb put-item \
    --table-name Teacher \
    --item '{
        "TeacherID": {"S": "T002"},
        "Name": {"S": "Prof. Robert Green"},
        "Age": {"N": "50"},
        "Department": {"S": "Chemistry"}
    }'

# Verify Data
aws dynamodb scan --table-name Person
aws dynamodb scan --table-name Student
aws dynamodb scan --table-name Teacher

