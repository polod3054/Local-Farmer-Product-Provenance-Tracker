# 🌾 Local Farmer Product Provenance Tracker

A blockchain-based solution for tracking and verifying local farm produce from harvest to consumer.

## 🎯 Features

- 🌱 Register produce batches with harvest details
- 📍 GPS location tracking
- 🚛 Transport event logging
- 🔍 Full produce traceability
- ✅ Ownership verification

## 📝 Contract Functions

### For Farmers

1. `register-batch`: Create a new produce batch with:
   - Product name
   - Harvest date
   - GPS coordinates
   - Farming method
   - Quantity

2. `update-batch-status`: Update the status of a batch

### For Transport Handlers

1. `add-transport-event`: Log transport events with:
   - Location
   - Temperature
   - Timestamp

### For Consumers

1. `get-batch-details`: View complete batch information
2. `get-transport-history`: Check the transport timeline
3. `verify-batch-ownership`: Verify produce authenticity

## 🚀 Getting Started

1. Deploy the contract using Clarinet
2. Register your first batch using `register-batch`
3. Track transport events with `add-transport-event`
4. Query batch information using read-only functions

## 💡 Use Cases

- Farmers can prove produce origin
- Consumers can verify authenticity
- Transport companies can log handling events
- Retailers can verify supply chain integrity

## 🔒 Security

- Owner-only batch updates
- Immutable harvest records
- Verified transport logging
```
