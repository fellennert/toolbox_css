import torch
from torch import nn
from transformers import BertTokenizer, BertModel
from torch.utils.data import Dataset, DataLoader


class SentenceDataset(Dataset):
    def __init__(self, texts, labels, tokenizer, max_len=128):
        self.texts = texts
        self.labels = labels
        self.tokenizer = tokenizer
        self.max_len = max_len
        
    def __len__(self):
        return len(self.texts)
    
    def __getitem__(self, idx):
        text = str(self.texts[idx])
        label = self.labels[idx]
        
        encoding = self.tokenizer.encode_plus(
            text,
            max_length=self.max_len,
            padding='max_length',
            truncation=True,
            return_tensors='pt'
        )
        
        return {
            'input_ids': encoding['input_ids'].flatten(),
            'attention_mask': encoding['attention_mask'].flatten(),
            'label': torch.tensor(label, dtype=torch.long)
        }

class BertClassifier(nn.Module):
    def __init__(self, dropout=0.1):
        super().__init__()
        self.bert = BertModel.from_pretrained('bert-base-uncased')
        self.dropout = nn.Dropout(dropout)
        self.classifier = nn.Linear(768, 2)
        
    def forward(self, input_ids, attention_mask):
        outputs = self.bert(input_ids=input_ids, attention_mask=attention_mask)
        pooled_output = outputs[1]
        pooled_output = self.dropout(pooled_output)
        return self.classifier(pooled_output)

def train_model(model, train_loader, val_loader, epochs=3, lr=2e-5):
    device = torch.device('mps' if torch.backends.mps.is_available() else 'cpu')
    model = model.to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=lr)
    criterion = nn.CrossEntropyLoss()
    
    for epoch in range(epochs):
        model.train()
        train_loss = 0
        for batch in train_loader:
            optimizer.zero_grad()
            input_ids = batch['input_ids'].to(device)
            attention_mask = batch['attention_mask'].to(device)
            labels = batch['label'].to(device)
            
            outputs = model(input_ids, attention_mask)
            loss = criterion(outputs, labels)
            loss.backward()
            optimizer.step()
            train_loss += loss.item()
            
        # Validation
        model.eval()
        val_loss = 0
        correct = 0
        total = 0
        
        with torch.no_grad():
            for batch in val_loader:
                input_ids = batch['input_ids'].to(device)
                attention_mask = batch['attention_mask'].to(device)
                labels = batch['label'].to(device)
                
                outputs = model(input_ids, attention_mask)
                loss = criterion(outputs, labels)
                val_loss += loss.item()
                
                _, predicted = torch.max(outputs, 1)
                total += labels.size(0)
                correct += (predicted == labels).sum().item()
        
        print(f'Epoch {epoch+1}:')
        print(f'Train Loss: {train_loss/len(train_loader):.4f}')
        print(f'Val Loss: {val_loss/len(val_loader):.4f}')
        print(f'Val Accuracy: {100*correct/total:.2f}%\n')

def predict(model, text, tokenizer):
    device = torch.device('mps' if torch.backends.mps.is_available() else 'cpu')
    model.eval()
    encoding = tokenizer.encode_plus(
        text,
        max_length=128,
        padding='max_length',
        truncation=True,
        return_tensors='pt'
    )
    
    input_ids = encoding['input_ids'].to(device)
    attention_mask = encoding['attention_mask'].to(device)
    
    with torch.no_grad():
        outputs = model(input_ids, attention_mask)
        _, predicted = torch.max(outputs, 1)
    
    return predicted.item()

# Usage example:
import pandas as pd
from sklearn.model_selection import train_test_split

imdb_reviews = pd.read_csv("files/imdb_reviews.csv")

# First split: separate test set (90% of data)
train_val_df, test_df = train_test_split(
    imdb_reviews,
    test_size=0.9,
    stratify=imdb_reviews['sentiment'],
    random_state=42
)
    
# Second split: separate train and validation (80/20 split of remaining data)
train_df, val_df = train_test_split(
    train_val_df,
    test_size=0.2,
    stratify=train_val_df['sentiment'],
    random_state=42
)
    
# Create feature/label pairs
X_train = train_df['text'].tolist()
y_train = train_df['sentiment'].tolist()
    
X_val = val_df['text'].tolist()
y_val = val_df['sentiment'].tolist()
    
X_test = test_df['text'].tolist()
y_test = test_df['sentiment'].tolist()

# Create label mapping
label_map = {'negative': 0, 'positive': 1}

# Convert labels to integers
y_train = [label_map[label] for label in y_train]
y_val = [label_map[label] for label in y_val]
y_test = [label_map[label] for label in y_test]

tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertClassifier()
    
# Recreate datasets with numeric labels
train_dataset = SentenceDataset(X_train, y_train, tokenizer)
val_dataset = SentenceDataset(X_val, y_val, tokenizer)
    
    # Create dataloaders
train_loader = DataLoader(train_dataset, batch_size=16, shuffle=True)
val_loader = DataLoader(val_dataset, batch_size=16)
    
    # Train model
train_model(model, train_loader, val_loader)
    
    # Make prediction
predict(model, "this is a hell of a movie", tokenizer)
predict(model, "this movie is hell", tokenizer)

prediction

# Or for all test texts:
test_predictions = [predict(model, text, tokenizer) for text in X_test]



from transformers import BertTokenizer

tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
tokens = tokenizer.tokenize('This is what tokenization in BERT looks like.')
print(tokens)  # Output: ['un', 'happi', 'ness']
